(********************************************************************************)
(*	Readconv.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Utility functions for converting {!Ast} values *to* {!Lambdoc_core} values.
*)

open Lambdoc_core


(********************************************************************************)
(**	{1 Submodule definitions}						*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Basic values}							*)
(********************************************************************************)

module Basic_input:
sig
	open Basic

	val expand_entity: string -> [ `Okay of string * string | `Error of Error.error_msg_t ]
	val matches_ident: string -> bool
	val bullet_of_string: string -> Bullet.t
	val numbering_of_string: string -> Numbering.t
	val floatation_of_string: string -> Floatation.t
end


(********************************************************************************)
(**	{2 Tabular values}							*)
(********************************************************************************)

module Tabular_input:
sig
	open Tabular

	val colspec_of_string: string -> colspec_t
	val cellspec_of_string: string -> cellspec_t
end


(********************************************************************************)
(**	{2 Order values}							*)
(********************************************************************************)

module Order_input:
sig
	open Basic
	open Order

	exception Invalid_order_format of string
	exception Invalid_order_levels of string * Level.hierarchical_t * int

	type ordinal_counter_t
	type hierarchical_counter_t

	val make_ordinal_counter: unit -> ordinal_counter_t ref
	val make_hierarchy_counter: unit -> hierarchical_counter_t ref

	val auto_ordinal: ordinal_counter_t ref -> [> `Auto_given of ordinal_t ]
	val auto_hierarchical: Level.hierarchical_t -> hierarchical_counter_t ref -> [> `Auto_given of hierarchical_t ]
	val user_ordinal: string -> [> `User_given of ordinal_t ]
	val user_hierarchical: Level.hierarchical_t -> string -> [> `User_given of hierarchical_t ]
	val no_order: unit -> [> `None_given ]
end


(********************************************************************************)
(**	{2 Math values}								*)
(********************************************************************************)

module Math_input:
sig
	open Math

	val from_mathtex: string -> t
	val from_mathml: string -> t
end

