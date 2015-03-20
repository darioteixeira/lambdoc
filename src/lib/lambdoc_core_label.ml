(********************************************************************************)
(*	Lambdoc_core_label.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core
open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type t =
	| Auto of Pointer.t
	| User of Pointer.t


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

let compare = Pervasives.compare

