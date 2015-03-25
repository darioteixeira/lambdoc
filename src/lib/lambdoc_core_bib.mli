(********************************************************************************)
(*	Lambdoc_core_bib.mli
	Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning bibliographic elements.
*)

module Inline = Lambdoc_core_inline
module Label = Lambdoc_core_label
module Order = Lambdoc_core_order


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

val make: Label.t -> order_t -> Inline.seq_t -> Inline.seq_t -> Inline.seq_t -> t

