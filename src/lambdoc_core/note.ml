(********************************************************************************)
(*	Implementation file for Note module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	A note is a record consisting of a label, an ordering, and the note's
	inline text.
*)

TYPE_CONV_PATH "Document"


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type t =
	{
	label: Label.t;
	order: Order.note_order_t;
	content: Block.M.nestable_frag_t;
	} (*with sexp*)

