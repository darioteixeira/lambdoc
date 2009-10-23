(********************************************************************************)
(*	Macromap.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core.Basic


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type key_t = raw_t
type value_t = int * Ast.seq_t
type t = (key_t, value_t) Hashtbl.t


(********************************************************************************)
(**	{2 Functions and values}						*)
(********************************************************************************)

let create () = Hashtbl.create 10

let add = Hashtbl.add

let mem = Hashtbl.mem

let find = Hashtbl.find

