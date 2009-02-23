(********************************************************************************)
(*	Implementation file for Bib module.

	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

TYPE_CONV_PATH "Bib"

open Basic


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type order_t = (Order.ordinal_t, Order.ordinal_t Order.auto_given_t) Order.t with sexp

type t =
	{
	label: Label.t;
	order: order_t;
	author: Inline.seq_t;
	title: Inline.seq_t;
	resource: Inline.seq_t;
	} with sexp


(********************************************************************************)
(**	{3 Functions and values}						*)
(********************************************************************************)

let bib label order author title resource =
	{
	label = label;
	order = order;
	author = Inline.get_seq author;
	title = Inline.get_seq title;
	resource = Inline.get_seq resource;
	}

