(********************************************************************************)
(*	Resource.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Sexplib


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type elt = Basic.alias_t
type t 


(********************************************************************************)
(**	{2 Functions and values}						*)
(********************************************************************************)

val empty: t
val is_empty: t -> bool
val mem: elt -> t -> bool
val add: elt -> t -> t

val t_of_sexp: Sexp.t -> t
val sexp_of_t: t -> Sexp.t

