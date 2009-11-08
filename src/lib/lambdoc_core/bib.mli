(********************************************************************************)
(*	Bib.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning bibliographic elements.
*)

open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
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
(**	{1 Public functions and values}						*)
(********************************************************************************)

val bib: Label.t -> order_t -> (_, _) Inline.t list -> (_, _) Inline.t list -> (_, _) Inline.t list -> t

