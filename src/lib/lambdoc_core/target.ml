(********************************************************************************)
(*	Target.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning document targets.
*)

TYPE_CONV_PATH "Target"


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type visible_target_t =
	| Custom_target of Custom.key_t * Custom.order_t
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
(**	{1 Functions and values}						*)
(********************************************************************************)

let custom envname order = Visible_target (Custom_target (envname, order))

let printout order = Visible_target (Wrapper_target (Wrapper.Printout, order))

let equation order = Visible_target (Wrapper_target (Wrapper.Equation, order))

let figure order = Visible_target (Wrapper_target (Wrapper.Figure, order))

let table order = Visible_target (Wrapper_target (Wrapper.Table, order))

let part order = Visible_target (Part_target order)

let section location order = Visible_target (Section_target (location, order))

let bib order = Bib_target order

let note order = Note_target order

