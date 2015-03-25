(********************************************************************************)
(*	Lambdoc_core_label.ml
	Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

module Basic = Lambdoc_core_basic

open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type t =
	| Auto of Pointer.t
	| User of Pointer.t
	with sexp


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

let compare = Pervasives.compare

