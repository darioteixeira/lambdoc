(********************************************************************************)
(*	Entity.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Utility functions for dealing with HTML entities.
*)

open Lambdoc_core

(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

val code_point: string -> Basic.unicode_t option
val iter: (string -> int -> unit) -> unit

