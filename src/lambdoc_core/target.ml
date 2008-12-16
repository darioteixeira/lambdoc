(********************************************************************************)
(*	Implementation file for Target module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions pertaining to internal document targets.
*)

TYPE_CONV_PATH "Document"


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

(**	The various types of wrappers.
*)
type wrapper_t =
	| Printout_wrapper
	| Equation_wrapper
	| Figure_wrapper
	| Table_wrapper
	(*with sexp*)


(**	The various variations of visible targets.
*)
type visible_target_t =
	| Section_target of Block.M.section_location_t * Block.M.section_order_t
	| Part_target of Block.M.part_order_t
	| Wrapper_target of wrapper_t * Block.M.wrapper_order_t
	(*with sexp*)


(**	At the highest level, a target can either be visible (if it can be
	referenced by [\ref], [\sref], or [\mref]), a bibliography block
	(referenced by [\cite]), or a note block (referenced by [\see]).
*)
type t =
	| Visible_target of visible_target_t
	| Bib_target of Block.M.bib_order_t
	| Note_target of Block.M.note_order_t
	(*with sexp*)


(********************************************************************************)
(**	{2 Public functions}							*)
(********************************************************************************)

let section_target location order = Visible_target (Section_target (location, order))

let part_target order = Visible_target (Part_target order)

let printout_target order = Visible_target (Wrapper_target (Printout_wrapper, order))

let equation_target order = Visible_target (Wrapper_target (Equation_wrapper, order))

let figure_target order = Visible_target (Wrapper_target (Figure_wrapper, order))

let table_target order = Visible_target (Wrapper_target (Table_wrapper, order))

let bib_target order = Bib_target order

let note_target order = Note_target order

