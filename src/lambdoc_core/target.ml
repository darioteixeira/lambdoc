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
	| Algorithm_wrapper
	| Equation_wrapper
	| Figure_wrapper
	| Table_wrapper
	(*with sexp*)


(**	The various variations of visible targets.
*)
type visible_target_t =
	| Section_target of Order.section_order_t
	| Appendix_target of Order.appendix_order_t
	| Preset_target of Order.preset_order_t
	| Part_target of Order.part_order_t
	| Wrapper_target of wrapper_t * Order.wrapper_order_t
	(*with sexp*)


(**	At the highest level, a target can either be visible (if it can be
	referenced by [\ref], [\sref], or [\mref]), a bibliography block
	(referenced by [\cite]), or a note block (referenced by [\see]).
*)
type t =
	| Visible_target of visible_target_t
	| Bib_target of Order.bib_order_t
	| Note_target of Order.note_order_t
	(*with sexp*)


(********************************************************************************)
(**	{2 Public functions}							*)
(********************************************************************************)

let section_target o = Visible_target (Section_target o)

let appendix_target o = Visible_target (Appendix_target o)

let preset_target o = Visible_target (Preset_target o)

let part_target o = Visible_target (Part_target o)

let algorithm_target o = Visible_target (Wrapper_target (Algorithm_wrapper, o))

let equation_target o = Visible_target (Wrapper_target (Equation_wrapper, o))

let figure_target o = Visible_target (Wrapper_target (Figure_wrapper, o))

let table_target o = Visible_target (Wrapper_target (Table_wrapper, o))

let bib_target o = Bib_target o

let note_target o = Note_target o

let is_none = function
	| Visible_target (Section_target `None_given)
	| Visible_target (Appendix_target `None_given)
	| Visible_target (Preset_target `None_given)	-> true
	| _						-> false