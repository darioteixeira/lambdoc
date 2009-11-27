(********************************************************************************)
(*	Custom.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Custom document blocks.
*)

TYPE_CONV_PATH "Custom"

open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type order_t = (Order.ordinal_t, [ Order.ordinal_t Order.auto_given_t | Order.user_given_t | Order.none_given_t ]) Order.t with sexp

type value_t = Design.t * raw_t with sexp

type key_t = string with sexp

type t = key_t * Label.t * order_t with sexp

