(********************************************************************************)
(*	Label.ml
	Copyright (c) 2009-2013 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type t =
	| Auto of Pointer.t
	| User of Pointer.t
	with sexp


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

let compare = Pervasives.compare

