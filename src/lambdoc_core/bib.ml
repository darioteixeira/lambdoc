(********************************************************************************)
(*	Implementation file for Bib module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	A bibliography entry is a record consisting of a label, an ordering,
	the title, the author, and the resource.
*)

TYPE_CONV_PATH "Document"


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type t =
	{
	label: Label.t;
	order: Order.ghost_order_t;
	title: Node.super_seq_t;
	author: Node.super_seq_t;
	resource: Node.super_seq_t;
	} (*with sexp*)

