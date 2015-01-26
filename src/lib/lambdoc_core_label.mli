(********************************************************************************)
(*	Lambdoc_core_label.mli
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

(**	Label identifiers can either be [Auto] (when they're automatically specified
	by the system) or [User] (when they're manually attributed by the user).
*)
type t =
	| Auto of Pointer.t
	| User of Pointer.t
	with sexp


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

(**	So it satisfies the [Map.OrderedType] signature.
*)
val compare: t -> t -> int

