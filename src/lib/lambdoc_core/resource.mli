(********************************************************************************)
(*	Resource.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Sexplib


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type elt = Basic.alias_t
type t 


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

val empty: t
val add: elt -> t -> t
val elements: t -> elt list

val t_of_sexp: Sexp.t -> t
val sexp_of_t: t -> Sexp.t

