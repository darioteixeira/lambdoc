(********************************************************************************)
(*	Implementation file for Order module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

TYPE_CONV_PATH "Document"

open ExtLib
open ExtString
open Basic


(********************************************************************************)
(**	{Exceptions}								*)
(********************************************************************************)

exception Invalid_number_of_levels of level_t * int
exception Invalid_appendix_string of string


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

(**	Ordinal ordering.
*)
type ordinal_t = int


(**	Ordinal counter.
*)
type ordinal_counter_t = ordinal_t


(**	Ordinal converters.
*)
type ordinal_converters_t = (string -> ordinal_t) * (ordinal_t -> string)


(**	Hierarchical ordering.
*)
type hierarchical_t =
	| Level1_order of int
	| Level2_order of int * int
	| Level3_order of int * int * int


(**	Hierarchical counter.
*)
type hierarchical_counter_t = int * int * int


(**	Hierarchical converters.
*)

type hierarchical_converters_t =
	{
	level1: (string -> int) * (int -> string);
	level2: (string -> int) * (int -> string);
	level3: (string -> int) * (int -> string);
	}


(**	A block's ordering can be assigned by any of three sources: [`Auto_given] means that
	the ordering should be automatically given by the system; [`User_given] means that the
	ordering is manually given by the user; finally, when the block should not have any
	ordering at all, [`None_given] is used.  Note that different classes of blocks allow
	a different subset of these ordering variants.  Moreover, only the first two variants
	must be parametrised over the actual ordering scheme used (as it makes no sense to talk
	of an ordering scheme when [`None_given] is used).
*)

type 'a auto_given_t = [ `Auto_given of 'a ] (*with sexp*)
type 'a user_given_t = [ `User_given of 'a ] (*with sexp*)
type none_given_t = [ `None_given ] (*with sexp*)
type ('a, 'b) given_t = 'b constraint 'b = [< 'a auto_given_t | 'a user_given_t | none_given_t ] (*with sexp*)


(**	Definition of the publicly visible ordering types.
*)

type section_order_t = (hierarchical_t as 'a, ['a auto_given_t | 'a user_given_t | none_given_t ]) given_t (*with sexp*)
type appendix_order_t = (hierarchical_t as 'a, ['a auto_given_t | 'a user_given_t | none_given_t ]) given_t (*with sexp*)
type preset_order_t = (hierarchical_t, none_given_t) given_t (*with sexp*)
type part_order_t = (ordinal_t as 'a, ['a auto_given_t | 'a user_given_t]) given_t (*with sexp*)
type wrapper_order_t = (ordinal_t as 'a, ['a auto_given_t | 'a user_given_t]) given_t (*with sexp*)
type bib_order_t = (ordinal_t as 'a, 'a auto_given_t) given_t (*with sexp*)
type note_order_t = (ordinal_t as 'a, 'a auto_given_t) given_t (*with sexp*)


(********************************************************************************)
(**	{2 Functions and values}						*)
(********************************************************************************)

(********************************************************************************)
(**	{3 Conversion functions}						*)
(********************************************************************************)

(**	This function converts a sequence of uppercase letters into its ordinal
	representation.  It is the inverse of {!alphaseq_of_int}.  Note that
	the maximum sequence length is capped at 3, which is far more than any
	reasonable document will require (18278 appendices should be enough
	for everybody).
*)
let int_of_alphaseq =
	let rex = Pcre.regexp ("^[A-Z]{1,3}$")
	in fun str -> match Pcre.pmatch ~rex str with
		| true ->
			let chars = List.rev (String.explode str) in
			let pow26 e = List.fold_left ( * ) 1 (List.make e 26) in
			let pos_value pos c = (pow26 pos) * ((int_of_char c) - 64) in
			let values = List.mapi pos_value chars
			in List.fold_left (+) 0 values
		| false ->
			raise (Invalid_appendix_string str)


(**	This function converts an ordinal number into a sequence of uppercase
	letters used for numbering appendices.  Ordinal 1 is converted to "A",
	26 to "Z", 27 to "AA", and so forth (note that this is not quite the
	same as conversion to base 26).  This function is the inverse of
	{!int_of_alphaseq}.
*)
let alphaseq_of_int num =
	let base = 26 in
	let rec from_base10 num =
		let num = num - 1
		in if num < base
		then
			[num]
		else
			let rem = num mod base
			and num = num / base
			in rem::(from_base10 num) in
	let alpha_of_int num =
		String.of_char (char_of_int (65 + num)) in
	let rems = from_base10 num
	in List.fold_left (^) "" (List.rev_map alpha_of_int rems)


(**	Converts an integer into its roman numeral representation.
*)
let roman_of_int i =
	let digit x y z = function
		| 1 -> [x]
		| 2 -> [x; x]
		| 3 -> [x; x; x]
		| 4 -> [x; y]
		| 5 -> [y]
		| 6 -> [y; x]
		| 7 -> [y; x; x]
		| 8 -> [y; x; x; x]
		| 9 -> [x; z]
		| _ -> invalid_arg "Invalid digit" in
	let rec to_roman i =
		if i <= 0
		then invalid_arg "Invalid roman numeral"
		else if i >= 1000
			then 'M' :: to_roman (i - 1000)
			else if i >= 100
				then digit 'C' 'D' 'M' (i / 100) @ to_roman (i mod 100)
				else if i >= 10
					then digit 'X' 'L' 'C' (i / 10) @ to_roman (i mod 10)
					else digit 'I' 'V' 'X' i
	in String.implode (to_roman i)


(********************************************************************************)
(**	{3 Predefined converters}						*)
(********************************************************************************)

let arabic_converters = (int_of_string, string_of_int)

let roman_converters = (int_of_string, roman_of_int)

let section_converters =
	{
	level1 = (int_of_string, string_of_int);
	level2 = (int_of_string, string_of_int);
	level3 = (int_of_string, string_of_int);
	}

let appendix_converters =
	{
	level1 = (int_of_alphaseq, alphaseq_of_int);
	level2 = (int_of_string, string_of_int);
	level3 = (int_of_string, string_of_int);
	}


(********************************************************************************)
(**	{3 Creation of counters}						*)
(********************************************************************************)

let make_ordinal_counter () = ref 0

let make_hierarchy_counter () = ref (0, 0, 0)


(********************************************************************************)
(**	{3 Constructors from counters}						*)
(********************************************************************************)

let incr_hierarchy_counter counter level =
	let (l1, l2, l3) = !counter
	in counter := match level with
		| Level1 -> (l1+1, 0, 0)
		| Level2 -> (l1, l2+1, 0)
		| Level3 -> (l1, l2, l3+1)


let ordinal_of_counter counter subpaged =
	match subpaged with
		| false ->
			let () = incr counter
			in `Auto_given !counter
		| true ->
			invalid_arg "Cannot break subpaging rules!"


let hierarchical_of_counter counter level subpaged =
	match subpaged with
		| false	->
			let () = incr_hierarchy_counter counter level in
			let (l1, l2, l3) = !counter
			in (match level with
				| Level1 -> `Auto_given (Level1_order l1)
				| Level2 -> `Auto_given (Level2_order (l1, l2))
				| Level3 -> `Auto_given (Level3_order (l1, l2, l3)))
		| true ->
			invalid_arg "Cannot break subpaging rules!"


let auto_section_order = hierarchical_of_counter
let auto_appendix_order = hierarchical_of_counter
let auto_part_order = ordinal_of_counter
let auto_wrapper_order = ordinal_of_counter
let auto_bib_order = ordinal_of_counter
let auto_note_order = ordinal_of_counter


(********************************************************************************)
(**	{3 Constructors from strings}						*)
(********************************************************************************)

let ordinal_of_string (conv, _) str subpaged =
	match subpaged with
		| true ->
			`User_given (conv str)
		| false ->
			invalid_arg "Cannot break subpaging rules!"


let hierarchical_of_string convs str level subpaged =
	let (conv1, _) = convs.level1
	and (conv2, _) = convs.level2
	and (conv3, _) = convs.level3
	in match subpaged with
		| true ->
			(match (level, String.nsplit str ".") with
				| (Level1, [a])		-> `User_given (Level1_order (conv1 a))
				| (Level2, [a; b])	-> `User_given (Level2_order (conv1 a, conv2 b))
				| (Level3, [a; b; c])	-> `User_given (Level3_order (conv1 a, conv2 b, conv3 c))
				| (expected, found)		-> raise (Invalid_number_of_levels (expected, List.length found)))
		| false ->
			invalid_arg "Cannot break subpaging rules!"


let user_section_order = hierarchical_of_string section_converters
let user_appendix_order = hierarchical_of_string appendix_converters
let user_part_order = ordinal_of_string roman_converters
let user_wrapper_order = ordinal_of_string arabic_converters


(********************************************************************************)
(**	{3 Constructors from nothing}						*)
(********************************************************************************)

let none_section_order _ = `None_given
let none_appendix_order _ = `None_given
let none_preset_order _ = `None_given


(********************************************************************************)
(**	{3 Checkers}								*)
(********************************************************************************)

let is_none = function
	| `Auto_given _	-> false
	| `User_given _	-> false
	| `None_given	-> true


(********************************************************************************)
(**	{3 Printers}								*)
(********************************************************************************)

let string_of_ordinal (_, of_ordinal) = function
	| `Auto_given o	-> of_ordinal o
	| `User_given o	-> of_ordinal o
	| `None_given	-> ""


let string_of_hierarchical converters ord =
	let (_, f1) = converters.level1
	and (_, f2) = converters.level2
	and (_, f3) = converters.level3 in
	let sprint_hierarchy = function
		| Level1_order l1		-> f1 l1
		| Level2_order (l1, l2)		-> (f1 l1) ^ "." ^ (f2 l2)
		| Level3_order (l1, l2, l3)	-> (f1 l1) ^ "." ^ (f2 l2) ^ "." ^ (f3 l3)
	in match ord with
		| `Auto_given o	-> sprint_hierarchy o
		| `User_given o	-> sprint_hierarchy o
		| `None_given	-> ""


let string_of_section_order = string_of_hierarchical section_converters
let string_of_appendix_order = string_of_hierarchical appendix_converters
let string_of_preset_order = string_of_hierarchical section_converters
let string_of_part_order = string_of_ordinal roman_converters
let string_of_wrapper_order = string_of_ordinal arabic_converters
let string_of_bib_order = string_of_ordinal arabic_converters
let string_of_note_order = string_of_ordinal arabic_converters

