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

exception Invalid_number_of_levels of hierarchical_level_t * int


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

(**	Ordinal ordering.
*)
type ordinal_t = int


(**	Ordinal counter.
*)
type ordinal_counter_t = ordinal_t


(**	Ordinal converter.
*)
type ordinal_converter_t = (ordinal_t -> string)


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

type hierarchical_converter_t =
	{
	level1: (int -> string);
	level2: (int -> string);
	level3: (int -> string);
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
type user_given_t = [ `User_given of string ] (*with sexp*)
type none_given_t = [ `None_given ] (*with sexp*)
type ('a, 'b) t = 'b constraint 'b = [< 'a auto_given_t | user_given_t | none_given_t ] (*with sexp*)


(********************************************************************************)
(**	{2 Functions and values}						*)
(********************************************************************************)

(********************************************************************************)
(**	{3 Conversion functions}						*)
(********************************************************************************)

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
(**	{3 Creation of counters}						*)
(********************************************************************************)

let make_ordinal_counter () = ref 0

let make_hierarchy_counter () = ref (0, 0, 0)


(********************************************************************************)
(**	{3 Constructors}							*)
(********************************************************************************)

let auto_ordinal counter =
	let () = incr counter
	in `Auto_given !counter


let auto_hierarchical level counter =
	let (l1, l2, l3) = match (level, !counter) with
		| (`Level1, (l1, _, _))		-> (l1+1, 0, 0)
		| (`Level2, (l1, l2, _))	-> (l1, l2+1, 0)
		| (`Level3, (l1, l2, l3))	-> (l1, l2, l3+1) in
	let () = counter := (l1, l2, l3)
	in match level with
		| `Level1 -> `Auto_given (Level1_order l1)
		| `Level2 -> `Auto_given (Level2_order (l1, l2))
		| `Level3 -> `Auto_given (Level3_order (l1, l2, l3))


let user_ordinal str = `User_given str


let user_hierarchical level str =
	match (level, List.length (String.nsplit str ".")) with
		| (`Level1, 1)
		| (`Level2, 2)
		| (`Level3, 3)		-> `User_given str
		| (expected, found)	-> raise (Invalid_number_of_levels (expected, found))


let none () = `None_given


(********************************************************************************)
(**	{3 Predefined converters}						*)
(********************************************************************************)

let arabic_converter = string_of_int

let roman_converter = roman_of_int

let mainbody_converter =
	{
	level1 = string_of_int;
	level2 = string_of_int;
	level3 = string_of_int;
	}

let appendixed_converter =
	{
	level1 = alphaseq_of_int;
	level2 = string_of_int;
	level3 = string_of_int;
	}


(********************************************************************************)
(**	{3 Printers}								*)
(********************************************************************************)

let string_of_ordinal conv = function
	| `Auto_given o	-> conv o
	| `User_given o	-> o
	| `None_given	-> ""


let string_of_hierarchical conv = function
	| `Auto_given (Level1_order l1)			-> conv.level1 l1
	| `Auto_given (Level2_order (l1, l2))		-> (conv.level1 l1) ^ "." ^ (conv.level2 l2)
	| `Auto_given (Level3_order (l1, l2, l3))	-> (conv.level1 l1) ^ "." ^ (conv.level2 l2) ^ "." ^ (conv.level3 l3)
	| `User_given o					-> o
	| `None_given					-> ""

