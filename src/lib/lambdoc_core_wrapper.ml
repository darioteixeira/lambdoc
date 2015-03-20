(********************************************************************************)
(*	Lambdoc_core_wrapper.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type order_t = (Order.ordinal_t, [ Order.ordinal_t Order.auto_given_t | Order.ordinal_t Order.user_given_t | Order.none_given_t ]) Order.t

type kind_t =
	| Printout
	| Equation
	| Figure
	| Table

type t =
	| Ordered of Label.t * (Order.ordinal_t, [ Order.ordinal_t Order.auto_given_t | Order.ordinal_t Order.user_given_t ]) Order.t * Inline.seq_t option
	| Unordered of Label.t * Inline.seq_t

