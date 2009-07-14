(********************************************************************************)
(*	Permissions.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Document permissions.
*)

open Lambdoc_core


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

val check_feature:
	?maybe_subpaged: bool option ->
	?maybe_wrapped: bool option ->
	(int * Error.error_msg_t) DynArray.t ->
	Ast.command_t ->
	Features.manuscript_feature_t ->
	unit

