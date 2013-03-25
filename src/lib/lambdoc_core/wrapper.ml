(********************************************************************************)
(*	Wrapper.ml
	Copyright (c) 2009-2013 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Sexplib.Std


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type order_t = (Order.ordinal_t, [ Order.ordinal_t Order.auto_given_t | Order.ordinal_t Order.user_given_t | Order.none_given_t ]) Order.t with sexp

type kind_t =
	| Printout
	| Equation
	| Figure
	| Table
	with sexp

type t =
	| Ordered of Label.t * (Order.ordinal_t, [ Order.ordinal_t Order.auto_given_t | Order.ordinal_t Order.user_given_t ]) Order.t * Inline.seq_t option
	| Unordered of Label.t * Inline.seq_t
	with sexp

