(********************************************************************************)
(*	Note.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

TYPE_CONV_PATH "Note"

open Basic


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type order_t = (Order.ordinal_t, Order.ordinal_t Order.auto_given_t) Order.t with sexp

type t =
	{
	label: Label.t;
	order: order_t;
	content: Block.frag_t;
	} with sexp


(********************************************************************************)
(**	{3 Functions and values}						*)
(********************************************************************************)

let note label order content =
	{
	label = label;
	order = order;
	content = Block.get_frag content;
	}

