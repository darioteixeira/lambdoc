(********************************************************************************)
(*	Interface file for Order module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions pertaining to document ordering.
*)

TYPE_CONV_PATH "Document"


(********************************************************************************)
(**	{2 Exceptions}								*)
(********************************************************************************)

exception Invalid_level_numbers of Level.t * int
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


(********************************************************************************)
(**	{3 Public values and functions}						*)
(********************************************************************************)

(********************************************************************************)
(**	{3 Predefined converters}						*)
(********************************************************************************)

val arabic_converters: ordinal_converters_t
val roman_converters: ordinal_converters_t
val section_converters: hierarchical_converters_t
val appendix_converters: hierarchical_converters_t


(********************************************************************************)
(**	{3 Printers}								*)
(********************************************************************************)

val string_of_ordinal: ordinal_converters_t -> t -> string
val string_of_hierarchical: hierarchical_converters_t -> t -> string


(********************************************************************************)
(**	{3 Creation of counters}						*)
(********************************************************************************)

val make_ordinal_counter: unit -> ordinal_counter_t ref
val make_hierarchy_counter: unit -> hierarchical_counter_t ref


(********************************************************************************)
(**	{3 {!t} constructors from counters}					*)
(********************************************************************************)

(**	Counters are an automatic source of numbering.  Therefore,
	all of these functions return an [`Auto_given] value.
*)

val ordinal_of_counter: ordinal_counter_t ref -> [> ordinal_scheme_t auto_given_t ]
val hierarchical_of_counter: Level.t -> hierarchical_counter_t ref -> [> hierarchical_scheme_t auto_given_t ]


(********************************************************************************)
(**	{3 {!t} constructors from strings}					*)
(********************************************************************************)

(**	Strings are provided by the users themselves.  Therefore,
	all of these functions return an [`User_given] value.
*)

val ordinal_of_string: ordinal_converters_t -> string -> [> ordinal_scheme_t user_given_t ]
val hierarchical_of_string: hierarchical_converters_t -> Level.t -> string -> [> hierarchical_scheme_t user_given_t ]


(********************************************************************************)
(**	{3 {!t} constructors from nothing}					*)
(********************************************************************************)

(**	Constructor used when no ordering is to be assigned.
	This function returns a [`None_given] value.
*)
val no_ordering: unit -> [> none_given_t ]


(********************************************************************************)
(**	{3 Top-level constructor functions}					*)
(********************************************************************************)

val sectional_order: body_sectional_order_t -> bool -> t
val appendix_order: appendix_sectional_order_t -> bool -> t
val preset_order: preset_sectional_order_t-> t
val algorithm_order: wrapper_order_t -> bool -> t
val equation_order: wrapper_order_t -> bool -> t
val figure_order: wrapper_order_t -> bool -> t
val table_order: wrapper_order_t -> bool -> t
val bib_order: ghost_order_t -> t
val note_order: ghost_order_t -> t

