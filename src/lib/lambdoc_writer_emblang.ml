(********************************************************************************)
(*	Lambdoc_writer_emblang.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type token_t =
	| Code
	| Plain of string


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

let convert expl = []

