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
(**	{2 Type definitions}							*)
(********************************************************************************)

type t =
	| Ent_name of string
	| Ent_deci of string
	| Ent_hexa of string


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

val code_point: t -> [ `Okay of Basic.entity_t | `Error of Error.error_msg_t ]
val iter: (string -> int -> unit) -> unit

