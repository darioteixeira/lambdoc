(********************************************************************************)
(*	Implementation file for Target module.

	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning document targets.
*)

TYPE_CONV_PATH "Target"


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type wrapper_kind_t =
	| Printout_wrapper
	| Equation_wrapper
	| Figure_wrapper
	| Table_wrapper
	with sexp

type visible_target_t =
	| Section_target of Heading.section_location_t * Heading.section_order_t
	| Part_target of Heading.part_order_t
	| Wrapper_target of wrapper_kind_t * Block.wrapper_order_t
	with sexp

type t =
	| Visible_target of visible_target_t
	| Bib_target of Bib.order_t
	| Note_target of Note.order_t
	with sexp


(********************************************************************************)
(**	{2 Functions and values}						*)
(********************************************************************************)

let section location order = Visible_target (Section_target (location, order))

let part order = Visible_target (Part_target order)

let printout order = Visible_target (Wrapper_target (Printout_wrapper, order))

let equation order = Visible_target (Wrapper_target (Equation_wrapper, order))

let figure order = Visible_target (Wrapper_target (Figure_wrapper, order))

let table order = Visible_target (Wrapper_target (Table_wrapper, order))

let bib order = Bib_target order

let note order = Note_target order

