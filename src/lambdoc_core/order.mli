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

exception Invalid_number_of_levels of hierarchical_level_t * int


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
type ('a, 'b) t = 'b constraint 'b = [< 'a auto_given_t | user_given_t | none_given_t ] (*with sexp*)


(********************************************************************************)
(**	{3 Public functions and values}						*)
(********************************************************************************)

(********************************************************************************)
(**	{3 Creation of counters}						*)
(********************************************************************************)

val make_ordinal_counter: unit -> ordinal_counter_t ref
val make_hierarchy_counter: unit -> hierarchical_counter_t ref


(********************************************************************************)
(**	{3 Constructors}							*)
(********************************************************************************)

val auto_ordinal: ordinal_counter_t ref -> [> `Auto_given of ordinal_t ]
val auto_hierarchical: hierarchical_level_t -> hierarchical_counter_t ref -> [> `Auto_given of hierarchical_t ]
val user_ordinal: string -> [> `User_given of string ]
val user_hierarchical: hierarchical_level_t -> string -> [> `User_given of string ]
val none: unit -> [> `None_given ]


(********************************************************************************)
(**	{3 Predefined converters}						*)
(********************************************************************************)

val arabic_converter: ordinal_converter_t
val roman_converter: ordinal_converter_t
val mainbody_converter: hierarchical_converter_t
val appendixed_converter: hierarchical_converter_t


(********************************************************************************)
(**	{3 Printers}								*)
(********************************************************************************)

val string_of_ordinal: ordinal_converter_t -> (ordinal_t, 'b) t -> string
val string_of_hierarchical: hierarchical_converter_t -> (hierarchical_t, 'b) t -> string

