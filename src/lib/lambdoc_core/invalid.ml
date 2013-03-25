(********************************************************************************)
(*	Invalid.ml
	Copyright (c) 2009-2013 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Prelude


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type t = Error.t nelist with sexp


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Constructors}							*)
(********************************************************************************)

let make errors = errors


(********************************************************************************)
(**	{2 Serialisation facilities}						*)
(********************************************************************************)

let serialize doc =
	Sexplib.Sexp.to_string_mach (sexp_of_t doc)

let deserialize str =
	t_of_sexp (Sexplib.Sexp.of_string str)

