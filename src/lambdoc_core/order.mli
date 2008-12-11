(********************************************************************************)
(*	Interface file for Order module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions pertaining to document ordering.
*)

open Basic


(********************************************************************************)
(**	{2 Exceptions}								*)
(********************************************************************************)

exception Invalid_number_of_levels of level_t * int


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type ordinal_t
type ordinal_counter_t
type ordinal_converter_t

type hierarchical_t
type hierarchical_counter_t
type hierarchical_converter_t

type 'a auto_given_t = [ `Auto_given of 'a ] (*with sexp*)
type user_given_t = [ `User_given of string ] (*with sexp*)
type none_given_t = [ `None_given ] (*with sexp*)
type ('a, 'b) given_t = 'b constraint 'b = [< 'a auto_given_t | user_given_t | none_given_t ] (*with sexp*)

type part_order_t = (ordinal_t as 'a, [ 'a auto_given_t | user_given_t | none_given_t ]) given_t (*with sexp*)
type section_order_t = (hierarchical_t as 'a, [ 'a auto_given_t | user_given_t | none_given_t ]) given_t (*with sexp*)
type wrapper_order_t = (ordinal_t as 'a, [ 'a auto_given_t | user_given_t ]) given_t (*with sexp*)
type bib_order_t = (ordinal_t as 'a, 'a auto_given_t) given_t (*with sexp*)
type note_order_t = (ordinal_t as 'a, 'a auto_given_t) given_t (*with sexp*)


(********************************************************************************)
(**	{3 Public functions and values}						*)
(********************************************************************************)

(********************************************************************************)
(**	{3 Creation of counters}						*)
(********************************************************************************)

val make_ordinal_counter: unit -> ordinal_counter_t ref
val make_hierarchy_counter: unit -> hierarchical_counter_t ref


(********************************************************************************)
(**	{3 Constructors from counters}						*)
(********************************************************************************)

val auto_part_order: ordinal_counter_t ref -> part_order_t
val auto_section_order: hierarchical_counter_t ref -> level_t -> section_order_t
val auto_wrapper_order: ordinal_counter_t ref -> wrapper_order_t
val auto_bib_order: ordinal_counter_t ref -> bib_order_t
val auto_note_order: ordinal_counter_t ref -> note_order_t


(********************************************************************************)
(**	{3 Constructors from strings}						*)
(********************************************************************************)

val user_part_order: string -> part_order_t
val user_section_order: string -> level_t -> section_order_t
val user_wrapper_order: string -> wrapper_order_t


(********************************************************************************)
(**	{3 Unit constructor}							*)
(********************************************************************************)

val none_order: unit -> [> `None_given ]


(********************************************************************************)
(**	{3 Predefined converters}						*)
(********************************************************************************)

val arabic_converter: ordinal_converter_t
val roman_converter: ordinal_converter_t
val section_converter: hierarchical_converter_t
val appendix_converter: hierarchical_converter_t


(********************************************************************************)
(**	{3 Printers}								*)
(********************************************************************************)

val string_of_section_order: ?conv:hierarchical_converter_t -> section_order_t -> string
val string_of_part_order: ?conv:ordinal_converter_t -> part_order_t -> string
val string_of_wrapper_order: ?conv:ordinal_converter_t -> wrapper_order_t -> string
val string_of_bib_order: ?conv:ordinal_converter_t -> bib_order_t -> string
val string_of_note_order: ?conv:ordinal_converter_t -> note_order_t -> string

