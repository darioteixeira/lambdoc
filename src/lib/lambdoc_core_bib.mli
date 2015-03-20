(********************************************************************************)
(*	Lambdoc_core_bib.mli
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning bibliographic elements.
*)

open Lambdoc_core


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type order_t = (Order.ordinal_t, Order.ordinal_t Order.auto_given_t) Order.t

type t =
	{
	label: Label.t;
	order: order_t;
	author: Inline.seq_t;
	title: Inline.seq_t;
	resource: Inline.seq_t;
	}


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

val make: Label.t -> order_t -> Inline.seq_t -> Inline.seq_t -> Inline.seq_t -> t

