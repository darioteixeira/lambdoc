(********************************************************************************)
(*	Implementation file for Bib module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	A bibliography entry is a record consisting of a label, an ordering,
	an author, a title, and a resource.
*)

TYPE_CONV_PATH "Document"


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type t =
	{
	label: Label.t;
	order: Block.bib_order_t;
	author: Node.seq_t;
	title: Node.seq_t;
	resource: Node.seq_t;
	} (*with sexp*)

