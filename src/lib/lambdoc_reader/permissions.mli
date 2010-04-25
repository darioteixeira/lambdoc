(********************************************************************************)
(*	Permissions.mli
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Document permissions.
*)

open Lambdoc_core


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

val check_feature:
	?maybe_minipaged: bool option ->
	?maybe_wrapped: bool option ->
	(int option * Error.error_msg_t) DynArray.t ->
	Ast.command_t ->
	Features.feature_t ->
	unit

