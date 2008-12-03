(********************************************************************************)
(*	Implementation file for Document_ghost.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of document ghost blocks.  A ghost block is one whose final
	position in the document is unrelated to where it was defined.  Ghost
	blocks are collected into a list, and output when a TOC or Bibliography
	declaration is made.
*)

TYPE_CONV_PATH "Document"

open Document_ref
open Document_node
open Document_block


(********************************************************************************)
(**	{2 Bibliography module}							*)
(********************************************************************************)

(**	A bibliography entry is a record consisting of a label, an ordering,
	the title, the author, and the resource.
*)
module Bib =
struct
	type t =
		{
		label: Label.t;
		order: Order.ghost_order_t;
		title: Node.super_seq_t;
		author: Node.super_seq_t;
		resource: Node.super_seq_t;
		} (*with sexp*)
end


(********************************************************************************)
(**	{2 Note module}								*)
(********************************************************************************)

(**	A note is a record consisting of a label, an ordering, and the note's
	inline text.
*)
module Note =
struct
	type t =
		{
		label: Label.t;
		order: Order.ghost_order_t;
		content: Block.nestable_frag_t;
		} (*with sexp*)
end

