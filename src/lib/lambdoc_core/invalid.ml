(********************************************************************************)
(*	Invalid.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

TYPE_CONV_PATH "Invalid"

open Prelude


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type manuscript_t = Error.t nelist with sexp
type composition_t = Error.t nelist with sexp


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

let make_manuscript errors = errors
let make_composition errors = errors

