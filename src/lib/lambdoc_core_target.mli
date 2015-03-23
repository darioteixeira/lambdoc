(********************************************************************************)
(*	Lambdoc_core_target.mli
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning document targets.
*)

module Bib = Lambdoc_core_bib
module Custom = Lambdoc_core_custom
module Heading = Lambdoc_core_heading
module Note = Lambdoc_core_note
module Wrapper = Lambdoc_core_wrapper


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type visible_target_t =
	| Custom_target of Custom.key_t * Custom.kind_t * Custom.order_t
	| Wrapper_target of Wrapper.kind_t * Wrapper.order_t
	| Part_target of Heading.part_order_t
	| Section_target of Heading.section_location_t * Heading.section_order_t
	with sexp

type t =
	| Visible_target of visible_target_t
	| Bib_target of Bib.order_t
	| Note_target of Note.order_t
	with sexp


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

val custom: Custom.key_t -> Custom.kind_t -> Custom.order_t -> t
val wrapper: Wrapper.kind_t -> Wrapper.order_t -> t
val part: Heading.part_order_t -> t
val section: Heading.section_location_t -> Heading.section_order_t -> t
val bib: Bib.order_t -> t
val note: Note.order_t -> t

