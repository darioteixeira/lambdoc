(********************************************************************************)
(*	Resource.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Sexplib


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

module M = Set.Make (struct type t = string let compare = Pervasives.compare end)


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

include M

let t_of_sexp sexp = match sexp with
	| Sexp.List l ->
		let adder res = function
			| Sexp.List _ -> Conv.of_sexp_error "Resource.t_of_sexp: atom needed" sexp
			| Sexp.Atom a -> add a res
		in List.fold_left adder empty l
	| Sexp.Atom _ ->
		Conv.of_sexp_error "Resource.t_of_sexp: list needed" sexp

let sexp_of_t res =
	Sexp.List (List.map (fun x -> Sexp.Atom x) (elements res))

