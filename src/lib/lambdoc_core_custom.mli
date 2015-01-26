(********************************************************************************)
(*	Lambdoc_core_custom.mli
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Custom document blocks.
*)

open Lambdoc_core
open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type kind_t =
	| Boxout
	| Theorem
	with sexp

type key_t = Pointer.t with sexp

type order_t = (Order.ordinal_t, [ Order.ordinal_t Order.auto_given_t | Order.ordinal_t Order.user_given_t | Order.none_given_t ]) Order.t with sexp

type anonymous_t = [ `Anonymous of key_t * Label.t ] with sexp

type unnumbered_t = [ `Unnumbered of key_t * Label.t ] with sexp

type numbered_t = [ `Numbered of key_t * Label.t * (Order.ordinal_t, [ Order.ordinal_t Order.auto_given_t | Order.ordinal_t Order.user_given_t | Order.none_given_t ]) Order.t ] with sexp

type t = [ anonymous_t | unnumbered_t | numbered_t ]


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

val anonymous: key_t -> Label.t -> order_t -> [> anonymous_t ]

val unnumbered: key_t -> Label.t -> order_t -> [> unnumbered_t ]

val numbered: key_t -> Label.t -> order_t -> [> numbered_t ]


(********************************************************************************)
(**	{1 Public modules}							*)
(********************************************************************************)

module Boxout:
sig
	type t = [ anonymous_t | unnumbered_t | numbered_t ] with sexp

	val make: [ anonymous_t | unnumbered_t | numbered_t ] -> t
end


module Theorem:
sig
	type t = [ unnumbered_t | numbered_t ] with sexp

	val make: [ anonymous_t | unnumbered_t | numbered_t ] -> t
end

