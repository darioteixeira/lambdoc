(********************************************************************************)
(*	Wrapper.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

TYPE_CONV_PATH "Wrapper"

open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type kind_t =
	| Printout
	| Equation
	| Figure
	| Table
	with sexp

type order_t = (Order.ordinal_t, [ Order.ordinal_t Order.auto_given_t | Order.ordinal_t Order.user_given_t ]) Order.t with sexp

type t = Label.t * order_t with sexp

