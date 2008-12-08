(********************************************************************************)
(*	Interface file for Order module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions pertaining to document ordering.
*)


(********************************************************************************)
(**	{2 Exceptions}								*)
(********************************************************************************)

exception Invalid_number_of_levels of Level.t * int
exception Invalid_appendix_string of string


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type ordinal_counter_t
type hierarchical_counter_t

type sectional_order_t
type appendix_order_t
type preset_order_t
type part_order_t
type wrapper_order_t
type bib_order_t
type note_order_t


(********************************************************************************)
(**	{3 Public values and functions}						*)
(********************************************************************************)

(********************************************************************************)
(**	{3 Creation of counters}						*)
(********************************************************************************)

val make_ordinal_counter: unit -> ordinal_counter_t ref
val make_hierarchy_counter: unit -> hierarchical_counter_t ref


(********************************************************************************)
(**	{3 Constructors from counters}						*)
(********************************************************************************)

val auto_sectional_order: hierarchical_counter_t ref -> Level.t -> bool -> sectional_order_t
val auto_appendix_order: hierarchical_counter_t ref -> Level.t -> bool -> appendix_order_t
val auto_part_order: ordinal_counter_t ref -> bool -> part_order_t
val auto_wrapper_order: ordinal_counter_t ref -> bool -> wrapper_order_t
val auto_bib_order: ordinal_counter_t ref -> bool -> bib_order_t
val auto_note_order: ordinal_counter_t ref -> bool -> note_order_t


(********************************************************************************)
(**	{3 Constructors from strings}						*)
(********************************************************************************)

val user_sectional_order: string -> Level.t -> bool -> sectional_order_t
val user_appendix_order: string -> Level.t -> bool -> appendix_order_t
val user_part_order: string -> bool -> part_order_t
val user_wrapper_order: string -> bool -> wrapper_order_t


(********************************************************************************)
(**	{3 Constructors from nothing}						*)
(********************************************************************************)

val none_sectional_order: unit -> bool -> sectional_order_t
val none_appendix_order: unit -> bool -> appendix_order_t
val none_preset_order: unit -> bool -> preset_order_t


(********************************************************************************)
(**	{3 Printers}								*)
(********************************************************************************)

val string_of_sectional_order: sectional_order_t -> string
val string_of_appendix_order: appendix_order_t -> string
val string_of_preset_order: preset_order_t -> string
val string_of_part_order: part_order_t -> string
val string_of_wrapper_order: wrapper_order_t -> string
val string_of_bib_order: bib_order_t -> string
val string_of_note_order: note_order_t -> string

