(********************************************************************************)
(*	Label.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions pertaining to document labels.
*)

open Sexplib.Std
open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

(**	Label identifiers can either be [`Auto_label] (when they're automatically
	specified by the system) or [`User_label] (when they're manually attributed
	by the user).
*)
type t =
	[ `Auto_label of Pointer.t
	| `User_label of Pointer.t
	] with sexp


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

(**	So it satisfies the [Map.OrderedType] signature.
*)
let compare = Pervasives.compare

