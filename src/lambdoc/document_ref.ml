(********************************************************************************)
(*	Implementation file for Document_ref.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions pertaining to internal document references and numbering.
*)

TYPE_CONV_PATH "Document"

open ExtList
open ExtString
open Document_basic


(********************************************************************************)
(**	{2 Label module}							*)
(********************************************************************************)

(**	The [Label] module encapsulates label-related definitions.
*)
module Label =
struct
	(**	Label identifiers can either be [`Auto_label] (when they're automatically
		specified by the system) or [`User_label] (when they're manually attributed
		by the user).
	*)
	type t =
		[ `Auto_label of ref_t
		| `User_label of ref_t
		] (*with sexp*)
end


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

	exception Invalid_appendix_string of string


	(************************************************************************)
	(**	{3 Public types}						*)
	(************************************************************************)

	(**	We support a three-level hierarchy, equivalent to XHTML's H1, H2, and H3.
	*)
	type hierarchy_t =
		private
		| Level1 of int
		| Level2 of int * int
		| Level3 of int * int * int


	(**	Ordinal counter.
	*)
	type ordinal_counter_t


	(**	Hierarchy counter.
	*)
	type hierarchy_counter_t


	(**	There are three different ordering schemes: [`Ordinal_scheme], [`Section_scheme],
		and [`Appendix_scheme].  The first is used, for example, for numbering wrappers and
		uses a single numeric counter.  The second is used exclusively for main body sections
		and has several numeric counters encoding an hierarchy.  As the last, it is used only
		in appendix sections and has a hierarchical structure whose first level uses an
		alphabetic character (only when displayed; internally it's still an integer).
	*)

	type ordinal_scheme_t = [ `Ordinal_scheme of int ] (*with sexp*)
	type section_scheme_t = [ `Section_scheme of hierarchy_t ] (*with sexp*)
	type appendix_scheme_t = [ `Appendix_scheme of hierarchy_t ] (*with sexp*)
	type 'a scheme_t = 'a constraint 'a = [< ordinal_scheme_t | section_scheme_t | appendix_scheme_t ] (*with sexp*)


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
	type ('a, 'b) order_t = 'b constraint 'b = [< 'a auto_given_t | 'a user_given_t | none_given_t ] (*with sexp*)


	(**	The ordering type for body sectional blocks.  It uses a [`Section_scheme],
		and allows for all giver variants.  There are two other restrictions to take
		into account: [`User_given] is only allowed in non-top level blocks, while
		non-top level blocks may only use [`User_given] or [`None_given] variants.
		These restrictions are not enforced by the type system; the type constructor
		takes care of them instead.
	*)
	type body_sectional_order_t = (section_scheme_t as 'a, ['a auto_given_t | 'a user_given_t | none_given_t ]) order_t (*with sexp*)


	(**	The ordering type for appendix sectional blocks.  It is mostly identical
		to the {!body_sectional_order_t}, the only difference being the use of an
		[`Appendix_scheme] instead of a [`Section_scheme].
	*)
	type appendix_sectional_order_t = (appendix_scheme_t as 'a, ['a auto_given_t | 'a user_given_t | none_given_t ]) order_t (*with sexp*)


	(**	The ordering type for preset sectional blocks (these are the TOC, the bibliography,
		and the list of notes).  Technically it uses a [`Section_scheme], though that is
		largely irrelevant because the only allowed given order is [`None_given].
	*)
	type preset_sectional_order_t = (section_scheme_t, none_given_t) order_t (*with sexp*)


	(**	The ordering type for wrapper blocks.  Wrappers use a ordinal scheme, and do
		not allow for the [`None_given] variant.  Furthermore, there are two additional
		restrictions to take into account: [`Auto_given] is only allowed outside of
		subpages, and wrappers inside of subpages may only use [`User_given].  These
		restrictions are enforced by the type constructor, not the type system.
	*)
	type wrapper_order_t = (ordinal_scheme_t as 'a, ['a auto_given_t | 'a user_given_t]) order_t (*with sexp*)


	(**	The ordering type for ghost blocks (bibliography entries and notes).
		The scheme is ordinal and only automatic numbering is allowed.
	*)
	type ghost_order_t = (ordinal_scheme_t as 'a, 'a auto_given_t) order_t (*with sexp*)


	(**	The various types of wrappers.
	*)
	type wrapper_t =
		private
		| Algorithm_wrapper
		| Equation_wrapper
		| Figure_wrapper
		| Table_wrapper
		(*with sexp*)


	(**	The various variations of orderings for visible blocks.
	*)
	type visible_order_t =
		private
		| Body_sectional_order of body_sectional_order_t
		| Appendix_sectional_order of appendix_sectional_order_t
		| Preset_sectional_order of preset_sectional_order_t
		| Wrapper_order of wrapper_t * wrapper_order_t
		(*with sexp*)


	(**	At the highest level, an ordered block can either be visible
		(if it can be reference by [\ref], [\sref], or [\mref]), a
		bibliography block (referenced by [\cite]), or a note block
		(referenced by [\see]).
	*)
	type t =
		private
		| Visible_order of visible_order_t
		| Bib_order of ghost_order_t
		| Note_order of ghost_order_t
		(*with sexp*)


	(************************************************************************)
	(**	{3 Public functions}						*)
	(************************************************************************)

	(************************************************************************)
	(**	{4 Printers}							*)
	(************************************************************************)

	val string_of_order: ('a, 'b) order_t -> string


	(************************************************************************)
	(**	{4 Creation of counters}					*)
	(************************************************************************)

	val make_ordinal_counter: unit -> ordinal_counter_t ref
	val make_hierarchy_counter: unit -> hierarchy_counter_t ref


	(************************************************************************)
	(**	{4 {!given_t} constructors from counters}			*)
	(************************************************************************)

	(**	Counters are an automatic source of numbering.  Therefore,
		all of these functions return an [`Auto_given] value.
	*)

	val ordinal_of_counter:		ordinal_counter_t ref ->	[> ordinal_scheme_t auto_given_t ]
	val section_of_counter:		hierarchy_counter_t ref ->	[> section_scheme_t auto_given_t ]
	val subsection_of_counter:	hierarchy_counter_t ref ->	[> section_scheme_t auto_given_t ]
	val subsubsection_of_counter:	hierarchy_counter_t ref ->	[> section_scheme_t auto_given_t ]
	val appendix_of_counter:	hierarchy_counter_t ref ->	[> appendix_scheme_t auto_given_t ]
	val subappendix_of_counter:	hierarchy_counter_t ref ->	[> appendix_scheme_t auto_given_t ]
	val subsubappendix_of_counter:	hierarchy_counter_t ref ->	[> appendix_scheme_t auto_given_t ]


	(************************************************************************)
	(**	{4 {!given_t} constructors from strings}			*)
	(************************************************************************)

	(**	Strings are provided by the users themselves.  Therefore,
		all of these functions return an [`User_given] value.
	*)

	val ordinal_of_string:		string -> [> ordinal_scheme_t user_given_t ]
	val section_of_string:		string -> [> section_scheme_t user_given_t ]
	val subsection_of_string:	string -> [> section_scheme_t user_given_t ]
	val subsubsection_of_string:	string -> [> section_scheme_t user_given_t ]
	val appendix_of_string:		string -> [> appendix_scheme_t user_given_t ]
	val subappendix_of_string:	string -> [> appendix_scheme_t user_given_t ]
	val subsubappendix_of_string:	string -> [> appendix_scheme_t user_given_t ]


	(************************************************************************)
	(**	{4 {!given_t} constructors from nothing}			*)
	(************************************************************************)

	(**	Constructor used when no ordering is to be assigned.
		This function returns a [`None_given] value.
	*)
	val no_ordering: unit -> [> none_given_t ]


	(************************************************************************)
	(**	{4 Top-level constructor functions}				*)
	(************************************************************************)

	val body_sectional_order: body_sectional_order_t -> bool -> t
	val appendix_sectional_order: appendix_sectional_order_t -> bool -> t
	val preset_sectional_order: preset_sectional_order_t-> t
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

	exception Invalid_appendix_string of string


	(************************************************************************)
	(**	{3 Public types}						*)
	(************************************************************************)

	type hierarchy_t =
		| Level1 of int
		| Level2 of int * int
		| Level3 of int * int * int
	
	type ordinal_counter_t = int

	type hierarchy_counter_t =
		{
		level1: int;
		level2: int;
		level3: int;
		}

	type ordinal_scheme_t = [ `Ordinal_scheme of int ] (*with sexp*)
	type section_scheme_t = [ `Section_scheme of hierarchy_t ] (*with sexp*)
	type appendix_scheme_t = [ `Appendix_scheme of hierarchy_t ] (*with sexp*)
	type 'a scheme_t = 'a constraint 'a = [< ordinal_scheme_t | section_scheme_t | appendix_scheme_t ] (*with sexp*)

	type 'a auto_given_t = [ `Auto_given of 'a scheme_t ] (*with sexp*)
	type 'a user_given_t = [ `User_given of 'a scheme_t ] (*with sexp*)
	type none_given_t = [ `None_given ] (*with sexp*)
	type ('a, 'b) order_t = 'b constraint 'b = [< 'a auto_given_t | 'a user_given_t | none_given_t ] (*with sexp*)

	type body_sectional_order_t = (section_scheme_t as 'a, ['a auto_given_t | 'a user_given_t | none_given_t ]) order_t (*with sexp*)
	type appendix_sectional_order_t = (appendix_scheme_t as 'a, ['a auto_given_t | 'a user_given_t | none_given_t ]) order_t (*with sexp*)
	type preset_sectional_order_t = (section_scheme_t, none_given_t) order_t (*with sexp*)
	type wrapper_order_t = (ordinal_scheme_t as 'a, ['a auto_given_t | 'a user_given_t]) order_t (*with sexp*)
	type ghost_order_t = (ordinal_scheme_t as 'a, 'a auto_given_t) order_t (*with sexp*)

	type wrapper_t =
		| Algorithm_wrapper
		| Equation_wrapper
		| Figure_wrapper
		| Table_wrapper
		(*with sexp*)

	type visible_order_t =
		| Body_sectional_order of body_sectional_order_t
		| Appendix_sectional_order of appendix_sectional_order_t
		| Preset_sectional_order of preset_sectional_order_t
		| Wrapper_order of wrapper_t * wrapper_order_t
		(*with sexp*)

	type t =
		| Visible_order of visible_order_t
		| Bib_order of ghost_order_t
		| Note_order of ghost_order_t
		(*with sexp*)


	(************************************************************************)
	(**	{3 Private functions}						*)
	(************************************************************************)

	(*	This function converts a sequence of uppercase letters into its ordinal
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


	let string_of_scheme = function
		| `Ordinal_scheme o				-> string_of_int o
		| `Section_scheme (Level1 l1)			-> string_of_int l1
		| `Section_scheme (Level2 (l1, l2))		-> (string_of_int l1) ^ "." ^ (string_of_int l2)
		| `Section_scheme (Level3 (l1, l2, l3))		-> (string_of_int l1) ^ "." ^ (string_of_int l2) ^ "." ^ (string_of_int l3)
		| `Appendix_scheme (Level1 l1)			-> alphaseq_of_int l1
		| `Appendix_scheme (Level2 (l1, l2))		-> (alphaseq_of_int l1) ^ "." ^ (string_of_int l2)
		| `Appendix_scheme (Level3 (l1, l2, l3))	-> (alphaseq_of_int l1) ^ "." ^ (string_of_int l2) ^ "." ^ (string_of_int l3)


	(************************************************************************)
	(**	{3 Public functions}						*)
	(************************************************************************)

	let string_of_order = function
		| `Auto_given s	-> string_of_scheme s
		| `User_given s	-> string_of_scheme s
		| `None_given	-> ""


	(************************************************************************)
	(**	{4 Creation of counters}					*)
	(************************************************************************)

	let make_ordinal_counter () = ref 0

	let make_hierarchy_counter () = ref {level1 = 0; level2 = 0; level3 = 0}


	(************************************************************************)
	(**	{4 {!given_t} constructors from counters}			*)
	(************************************************************************)

	let ordinal_of_counter c =
		incr c;
		`Auto_given (`Ordinal_scheme !c)

	let section_of_counter c =
		c := {level1 = !c.level1+1; level2 = 0; level3 = 0;};
		`Auto_given (`Section_scheme (Level1 !c.level1))

	let subsection_of_counter c =
		c := {!c with level2 = !c.level2+1; level3 = 0;};
		`Auto_given (`Section_scheme (Level2 (!c.level1, !c.level2)))

	let subsubsection_of_counter c =
		c := {!c with level3 = !c.level3+1;};
		`Auto_given (`Section_scheme (Level3 (!c.level1, !c.level2, !c.level3)))

	let appendix_of_counter c =
		c := {level1 = !c.level1+1; level2 = 0; level3 = 0;};
		`Auto_given (`Appendix_scheme (Level1 !c.level1))

	let subappendix_of_counter c =
		c := {!c with level2 = !c.level2+1; level3 = 0;};
		`Auto_given (`Appendix_scheme (Level2 (!c.level1, !c.level2)))

	let subsubappendix_of_counter c =
		c := {!c with level3 = !c.level3+1;};
		`Auto_given (`Appendix_scheme (Level3 (!c.level1, !c.level2, !c.level3)))


	(************************************************************************)
	(**	{4 {!given_t} constructors from strings}			*)
	(************************************************************************)

	let counters_of_string funcs s =
		let parts = String.nsplit s "."
		in List.map2 (fun func part -> func part) funcs parts

	let ordinal_of_string s =
		`User_given (`Ordinal_scheme (int_of_string s))

	let section_of_string s =
		`User_given (`Section_scheme (Level1 (int_of_string s)))

	let subsection_of_string s =
		let parts = counters_of_string [int_of_string; int_of_string] s
		in match parts with
			| [a; b] 	-> `User_given (`Section_scheme (Level2 (a, b)))
			| _		-> failwith "Unexpected list length"
		
	let subsubsection_of_string s =
		let parts = counters_of_string [int_of_string; int_of_string; int_of_string] s
		in match parts with
			| [a; b; c] 	-> `User_given (`Section_scheme (Level3 (a, b, c)))
			| _		-> failwith "Unexpected list length"

	let appendix_of_string s =
		`User_given (`Appendix_scheme (Level1 (int_of_alphaseq s)))

	let subappendix_of_string s =
		let parts = counters_of_string [int_of_alphaseq; int_of_string] s
		in match parts with
			| [a; b] 	-> `User_given (`Appendix_scheme (Level2 (a, b)))
			| _		-> failwith "Unexpected list length"

	let subsubappendix_of_string s =
		let parts = counters_of_string [int_of_alphaseq; int_of_string; int_of_string] s
		in match parts with
			| [a; b; c] 	-> `User_given (`Appendix_scheme (Level3 (a, b, c)))
			| _		-> failwith "Unexpected list length"


	(************************************************************************)
	(**	{4 {!given_t} constructors from nothing}			*)
	(************************************************************************)

	let no_ordering () = `None_given


	(************************************************************************)
	(**	{4 Top-level constructor functions}				*)
	(************************************************************************)

	let body_sectional_order o is_top = match (o, is_top) with
		| (`User_given _, false)
		| (`Auto_given _, true)
		| (`None_given, _)	-> Visible_order (Body_sectional_order o)
		| _			-> failwith "Does not satisfy body_sectional_order rules!"

	let appendix_sectional_order o is_top = match (o, is_top) with
		| (`User_given _, false)
		| (`Auto_given _, true)
		| (`None_given, _)	-> Visible_order (Appendix_sectional_order o)
		| _			-> failwith "Does not satisfy appendix_sectional_order rules!"

	let preset_sectional_order o =
		Visible_order (Preset_sectional_order o)

	let wrapper_order wrapper o is_top = match (o, is_top) with
		| (`User_given _, false)
		| (`Auto_given _, true)	-> Visible_order (Wrapper_order (wrapper, o))
		| _			-> failwith "Does not satisfy wrapper_order rules!"

	let algorithm_order =
		wrapper_order Algorithm_wrapper

	let equation_order =
		wrapper_order Equation_wrapper

	let figure_order = 
		wrapper_order Figure_wrapper

	let table_order = 
		wrapper_order Table_wrapper

	let bib_order o =
		Bib_order o

	let note_order o =
		Note_order o
end


(********************************************************************************)
(**	{2 Label_dict module}							*)
(********************************************************************************)

(**	The label dictionary contains a mapping between the labels used in the
	document and the ordering of the corresponding block.  Note that all
	labels share the same namespace.  Users are therefore encouraged to
	use the informal LaTeX convention of prefixing each label with [fig:],
	[tab:], [sec:], etc.
*)
module Label_dict =
struct
	type t = (Label.t, Order.t) Hashtbl.t (*with sexp*)
end

