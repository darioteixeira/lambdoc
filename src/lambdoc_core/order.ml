(********************************************************************************)
(*	Implementation file for Document_order.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions pertaining to document numbering.
*)

TYPE_CONV_PATH "Document"

open ExtList
open ExtString
open Document_basic
open Document_level


(********************************************************************************)
(**	{2 Order module}							*)
(********************************************************************************)

(**	The [Order] module encapsulates order-related definitions.
*)
module Order:
sig
	(************************************************************************)
	(**	{3 Exceptions}							*)
	(************************************************************************)

	exception Invalid_level_numbers of Level.t * int
	exception Invalid_appendix_string of string


	(************************************************************************)
	(**	{3 Public types}						*)
	(************************************************************************)

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


	(**	There are two different ordering schemes: [`Ordinal_scheme], and [`Hierarchical_scheme].
		The first is used, for example, for numbering wrappers and uses a single numeric counter.
		The second is used for sections and has several numeric counters encoding an hierarchy.
	*)

	type ordinal_scheme_t = [ `Ordinal_scheme of ordinal_t ] (*with sexp*)
	type hierarchical_scheme_t = [ `Hierarchical_scheme of hierarchical_t ] (*with sexp*)
	type 'a scheme_t = 'a constraint 'a = [< ordinal_scheme_t | hierarchical_scheme_t ] (*with sexp*)


	(**	A block's ordering can be assigned by any of three sources: [`Auto_given] means that
		the ordering should be automatically given by the system; [`User_given] means that the
		ordering is manually given by the user; finally, when the block should not have any
		ordering at all, [`None_given] is used.  Note that different classes of blocks allow
		a different subset of these ordering variants.  Moreover, the first two variants must
		be parametrised over the actual ordering scheme used (as it makes no sense to talk of
		an ordering scheme when [`None_given] is used).
	*)

	type 'a auto_given_t = [ `Auto_given of 'a scheme_t ] (*with sexp*)
	type 'a user_given_t = [ `User_given of 'a scheme_t ] (*with sexp*)
	type none_given_t = [ `None_given ] (*with sexp*)
	type ('a, 'b) t = 'b constraint 'b = [< 'a auto_given_t | 'a user_given_t | none_given_t ] (*with sexp*)


	(**	The ordering type for sectional blocks in the main document body.  It uses a
		[`Hierarchical_scheme], and allows for all giver variants.  There are two other
		restrictions to take into account: [`User_given] is only allowed in non-toplevel
		blocks, while non-top level blocks may only use [`User_given] or [`None_given]
		variants.  These restrictions are not enforced by the type system; the type
		constructor takes care of them instead.
	*)
	type sectional_order_t = (hierarchical_scheme_t as 'a, ['a auto_given_t | 'a user_given_t | none_given_t ]) t (*with sexp*)


	(**	The ordering type for sectional blocks in the document appendix.
		It is identical to the {!sectional_order_t}.
	*)
	type appendix_order_t = (hierarchical_scheme_t as 'a, ['a auto_given_t | 'a user_given_t | none_given_t ]) t (*with sexp*)


	(**	The ordering type for preset sectional blocks (these are the TOC, the bibliography,
		and the list of notes).  Technically it uses a [`Hierarchical_scheme], though that is
		largely irrelevant because the only allowed given order is [`None_given].
	*)
	type preset_order_t = (hierarchical_scheme_t, none_given_t) t (*with sexp*)


	(**	The ordering type for wrapper blocks.  Wrappers use a ordinal scheme, and do
		not allow for the [`None_given] variant.  Furthermore, there are two additional
		restrictions to take into account: [`Auto_given] is only allowed outside of
		subpages, and wrappers inside of subpages may only use [`User_given].  These
		restrictions are enforced by the type constructor, not the type system.
	*)
	type wrapper_order_t = (ordinal_scheme_t as 'a, ['a auto_given_t | 'a user_given_t]) t (*with sexp*)


	(**	The ordering type for ghost blocks (bibliography entries and notes).
		The scheme is ordinal and only automatic numbering is allowed.
	*)
	type ghost_order_t = (ordinal_scheme_t as 'a, 'a auto_given_t) t (*with sexp*)


	(************************************************************************)
	(**	{3 Public values and functions}					*)
	(************************************************************************)

	(************************************************************************)
	(**	{4 Predefined converters}					*)
	(************************************************************************)

	val arabic_converters: ordinal_converters_t
	val roman_converters: ordinal_converters_t
	val section_converters: hierarchical_converters_t
	val appendix_converters: hierarchical_converters_t


	(************************************************************************)
	(**	{4 Printers}							*)
	(************************************************************************)

	val string_of_ordinal: ordinal_converters_t -> t -> string
	val string_of_hierarchical: hierarchical_converters_t -> t -> string


	(************************************************************************)
	(**	{4 Creation of counters}					*)
	(************************************************************************)

	val make_ordinal_counter: unit -> ordinal_counter_t ref
	val make_hierarchy_counter: unit -> hierarchical_counter_t ref


	(************************************************************************)
	(**	{4 {!t} constructors from counters}				*)
	(************************************************************************)

	(**	Counters are an automatic source of numbering.  Therefore,
		all of these functions return an [`Auto_given] value.
	*)

	val ordinal_of_counter: ordinal_counter_t ref -> [> ordinal_scheme_t auto_given_t ]
	val hierarchical_of_counter: Level.t -> hierarchical_counter_t ref -> [> hierarchical_scheme_t auto_given_t ]


	(************************************************************************)
	(**	{4 {!t} constructors from strings}				*)
	(************************************************************************)

	(**	Strings are provided by the users themselves.  Therefore,
		all of these functions return an [`User_given] value.
	*)

	val ordinal_of_string: ordinal_converters_t -> string -> [> ordinal_scheme_t user_given_t ]
	val hierarchical_of_string: hierarchical_converters_t -> Level.t -> string -> [> hierarchical_scheme_t user_given_t ]


	(************************************************************************)
	(**	{4 {!t} constructors from nothing}				*)
	(************************************************************************)

	(**	Constructor used when no ordering is to be assigned.
		This function returns a [`None_given] value.
	*)
	val no_ordering: unit -> [> none_given_t ]


	(************************************************************************)
	(**	{4 Top-level constructor functions}				*)
	(************************************************************************)

	val sectional_order: body_sectional_order_t -> bool -> t
	val appendix_order: appendix_sectional_order_t -> bool -> t
	val preset_order: preset_sectional_order_t-> t
	val algorithm_order: wrapper_order_t -> bool -> t
	val equation_order: wrapper_order_t -> bool -> t
	val figure_order: wrapper_order_t -> bool -> t
	val table_order: wrapper_order_t -> bool -> t
	val bib_order: ghost_order_t -> t
	val note_order: ghost_order_t -> t
end =
struct
	(************************************************************************)
	(**	{3 Exceptions}							*)
	(************************************************************************)

	exception Invalid_level_numbers of Level.t * int
	exception Invalid_appendix_string of string


	(************************************************************************)
	(**	{3 Public types}						*)
	(************************************************************************)

	type ordinal_t = int

	type ordinal_counter_t = ordinal_t

	type ordinal_converters_t = (string -> ordinal_t) * (ordinal_t -> string)

	type hierarchy_t = 
		| Level1_order of int
		| Level2_order of int * int
		| Level3_order of int * int * int

	type hierarchical_counter_t = int * int * int

	type hierarchical_converters_t =
		{
		level1: (string -> int) * (int -> string);
		level2: (string -> int) * (int -> string);
		level3: (string -> int) * (int -> string);
		}

	type ordinal_scheme_t = [ `Ordinal_scheme of ordinal_t ] (*with sexp*)
	type hierarchical_scheme_t = [ `Hierarchical_scheme of hierarchy_t ] (*with sexp*)
	type 'a scheme_t = 'a constraint 'a = [< ordinal_scheme_t | hierarchical_scheme_t ] (*with sexp*)

	type 'a auto_given_t = [ `Auto_given of 'a scheme_t ] (*with sexp*)
	type 'a user_given_t = [ `User_given of 'a scheme_t ] (*with sexp*)
	type none_given_t = [ `None_given ] (*with sexp*)
	type ('a, 'b) t = 'b constraint 'b = [< 'a auto_given_t | 'a user_given_t | none_given_t ] (*with sexp*)

	type sectional_order_t = (hierarchical_scheme_t as 'a, ['a auto_given_t | 'a user_given_t | none_given_t ]) t (*with sexp*)
	type appendix_order_t = (hierarchical_scheme_t as 'a, ['a auto_given_t | 'a user_given_t | none_given_t ]) t (*with sexp*)
	type preset_order_t = (hierarchical_scheme_t, none_given_t) t (*with sexp*)
	type wrapper_order_t = (ordinal_scheme_t as 'a, ['a auto_given_t | 'a user_given_t]) t (*with sexp*)
	type ghost_order_t = (ordinal_scheme_t as 'a, 'a auto_given_t) t (*with sexp*)


	(************************************************************************)
	(**	{3 Private functions}						*)
	(************************************************************************)

	(**	Increments a hierarchical counter.
	*)
	let incr_hierarchy_counter level counter =
		counter := match level with
			| Level.Level1 -> let (level1, level2, level3) = !counter in (level1+1, 0, 0)
			| Level.Level2 -> let (level1, level2, level3) = !counter in (level1, level2+1, 0)
			| Level.Level3 -> let (level1, level2, level3) = !counter in (level1, level2, level3+1)


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


	(************************************************************************)
	(**	{3 Public functions}						*)
	(************************************************************************)

	(************************************************************************)
	(**	{4 Predefined converters}					*)
	(************************************************************************)

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


	(************************************************************************)
	(**	{4 Printers}							*)
	(************************************************************************)

	let string_of_ordinal (_, of_ordinal) = function
		| `Auto_given o	-> of_ordinal o
		| `User_given o	-> of_ordinal o
		| `None_given	-> ""

	let string_of_hierarchical converters scheme =
		let (_, f1) = converters.level1
		and (_, f2) = converters.level2
		and (_, f3) = converters.level3 in
		let sprint_hierarchy = function
			| Level1_order l1		-> f1 l1
			| Level2_order (l1, l2)		-> (f1 l1) ^ "." ^ (f2 l2)
			| Level3_order (l1, l2, l3)	-> (f1 l1) ^ "." ^ (f2 l2) ^ "." ^ (f3 l3)
		in match scheme with
			| `Auto_given o	-> sprint_hierarchy o
			| `User_given o	-> sprint_hierarchy o
			| `None_given	-> ""


	(************************************************************************)
	(**	{4 Creation of counters}					*)
	(************************************************************************)

	let make_ordinal_counter () = ref 0

	let make_hierarchy_counter () = ref (0, 0, 0)


	(************************************************************************)
	(**	{4 {!t} constructors from counters}				*)
	(************************************************************************)

	let ordinal_of_counter counter =
		let () = incr counter
		in `Auto_given (`Ordinal_scheme !counter)

	let section_of_counter level counter =
		let () = incr_hierarchy_counter level counter
		in match level with
			| Level.Level1 -> let (l1, _, _) = !counter in `Auto_given (`Hierarchical_scheme (Level1_order l1))
			| Level.Level2 -> let (l1, l2, _) = !counter in `Auto_given (`Hierarchical_scheme (Level2_order (l1, l2)))
			| Level.Level3 -> let (l1, l2, l3) = !counter in `Auto_given (`Hierarchical_scheme (Level3_order (l1, l2, l3)))

	let appendix_of_counter level counter =
		let () = incr_hierarchy_counter level counter
		in match level with
			| Level.Level1 -> let (l1, _, _) = !counter in `Auto_given (`Appendix_scheme (Level1_order l1))
			| Level.Level2 -> let (l1, l2, _) = !counter in `Auto_given (`Appendix_scheme (Level2_order (l1, l2)))
			| Level.Level3 -> let (l1, l2, l3) = !counter in `Auto_given (`Appendix_scheme (Level3_order (l1, l2, l3)))


	(************************************************************************)
	(**	{4 {!t} constructors from strings}				*)
	(************************************************************************)

	let ordinal_scheme_of_string str =
		`User_given (`Ordinal_scheme (int_of_string str))

	let section_scheme_of_string level str =
		match (level, String.nsplit str ".") with
			| (Level.Level1, [a])		-> `User_given (`Hierarchical_scheme (Level1_order (int_of_string a)))
			| (Level.Level2, [a; b])	-> `User_given (`Hierarchical_scheme (Level2_order (int_of_string a, int_of_string b)))
			| (Level.Level3, [a; b; c])	-> `User_given (`Hierarchical_scheme (Level3_order (int_of_string a, int_of_string b, int_of_string c)))
			| (expected, found)		-> raise (Invalid_level_numbers (expected, List.length found))

	let appendix_scheme_of_string level str =
		match (level, String.nsplit str ".") with
			| (Level.Level1, [a])		-> `User_given (`Appendix_scheme (Level1_order (int_of_alphaseq a)))
			| (Level.Level2, [a; b])	-> `User_given (`Appendix_scheme (Level2_order (int_of_alphaseq a, int_of_string b)))
			| (Level.Level3, [a; b; c])	-> `User_given (`Appendix_scheme (Level3_order (int_of_alphaseq a, int_of_string b, int_of_string c)))
			| (expected, found)		-> raise (Invalid_level_numbers (expected, List.length found))


	(************************************************************************)
	(**	{4 {!t} constructors from nothing}				*)
	(************************************************************************)

	let no_ordering () = `None_given


	(************************************************************************)
	(**	{4 Top-level constructor functions}				*)
	(************************************************************************)

	let sectional_order o subpaged = match (o, subpaged) with
		| (`Auto_given _, false)
		| (`User_given _, true)
		| (`None_given, _)	-> o
		| _			-> failwith "Does not satisfy sectional_order rules!"

	let appendix_order o subpaged = match (o, subpaged) with
		| (`Auto_given _, false)
		| (`User_given _, true)
		| (`None_given, _)	-> o
		| _			-> failwith "Does not satisfy appendix_order rules!"

	let preset_order o = o

	let wrapper_order wrapper o subpaged = match (o, subpaged) with
		| (`Auto_given _, false)
		| (`User_given _, true)	-> o
		| _			-> failwith "Does not satisfy wrapper_order rules!"

	let algorithm_order = wrapper_order 

	let equation_order = wrapper_order

	let figure_order = wrapper_order

	let table_order = wrapper_order

	let bib_order o = o

	let note_order o = o
end

