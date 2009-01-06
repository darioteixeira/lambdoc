(********************************************************************************)
(*	Interface file for Permissions module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

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
	Ast.M.command_t ->
	Features.manuscript_feature_t ->
	unit

