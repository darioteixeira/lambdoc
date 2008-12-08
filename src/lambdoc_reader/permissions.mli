(********************************************************************************)
(*	Interface file for Permissions module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Document permissions.
*)

(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

val check_command_feature:
	(int * Error.error_msg_t) DynArray.t ->
	Ast.M.command_t ->
	bool option ->
	Features.command_feature_t ->
	unit

