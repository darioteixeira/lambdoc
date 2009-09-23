(********************************************************************************)
(*	Resource.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Sexplib


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

module M = Set.Make (struct type t = string let compare = Pervasives.compare end)


(********************************************************************************)
(**	{2 Functions and values}						*)
(********************************************************************************)

include M

let t_of_sexp = function
	| Sexp.List l -> empty
	| Sexp.Atom _ as sexp -> Conv.of_sexp_error "Resource.t_of_sexp: list needed" sexp

let sexp_of_t set =
	Sexp.List []

