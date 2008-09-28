(********************************************************************************)
(**	Document permissions.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Permission module}							*)
(********************************************************************************)

(**	The [Permission] module declares the various permission classes for
	command parameters and provides functions to verify their compliance.
*)
module Permission =
struct
	(**	The type encoding the various kinds of available permissions.
	*)
	type t =
		| Optional		(** The parameter is optional but may not be empty. *)
		| Optional0		(** The parameter is optional and may be empty. *)
		| Mandatory		(** The parameter is mandatory and may not be empty. *)
		| Mandatory0		(** The parameter is mandatory but may be empty. *)
		| Forbidden		(** The parameter is forbidden, either empty or not. *)
		| Forbidden0		(** The parameter is forbidden, unless it is empty. *)


	(**	The following values/functions encode the predefined permissions for
		the various classes of commands.  Each permission class is a 4-tuple
		stating the individual permissions for the label, ordering, extra, and
		secondary parameters, respectively.  While most classes are constant,
		some of them are context-sensitive and are therefore functions.
	*)

	let forbidden_class =
		(Forbidden, Forbidden, Forbidden, Forbidden)

	let user_sectional_class subpaged =
		let perm_order = if subpaged then Mandatory0 else Forbidden0
		in (Optional, perm_order, Forbidden, Forbidden)

	let preset_sectional_class =
		(Optional, Forbidden, Forbidden, Forbidden)

	let math_class =
		(Forbidden, Forbidden, Forbidden, Mandatory)

	let tabular_class =
		(Forbidden, Forbidden, Forbidden, Mandatory)

	let listing_class =
		(Forbidden, Forbidden, Optional, Forbidden)

	let quote_class =
		(Forbidden, Forbidden, Optional, Forbidden)

	let floater_class perm_secondary subpaged captioned =
		let perm_order = match (subpaged, captioned) with
			| (true, true)		-> Mandatory
			| (true, false)		-> Mandatory0
			| (false, true)		-> Forbidden
			| (false, false)	-> Forbidden0
		in (Optional, perm_order, Optional, perm_secondary)

	let algorithm_class = floater_class Mandatory0

	let equation_class = floater_class Forbidden

	let figure_class = floater_class Mandatory

	let table_class = floater_class Forbidden

	let ghost_class = (Optional, Forbidden, Forbidden, Forbidden)


	(*	This function checks whether a parameter is valid given its
		associated permission.  It returns an optional value stating
		the reason why the parameter was deemed invalid.  A [None]
		result indicates the parameter is valid.
	*)
	let reason_why_invalid perm = function
		| Some "" -> (match perm with
			| Optional0
			| Mandatory0
			| Forbidden0	-> None
			| Optional
			| Mandatory	-> Some Error.Reason_is_empty_when_non_empty_mandatory
			| Forbidden	-> Some Error.Reason_is_empty_when_forbidden)
		| Some other -> (match perm with
			| Forbidden
			| Forbidden0	-> Some (Error.Reason_is_non_empty_when_forbidden other)
			| _		-> None)
		| None -> (match perm with
			| Mandatory0
			| Mandatory	-> Some Error.Reason_is_absent_when_mandatory
			| _		-> None)


	(*	This function goes through all the command parameters, checking
		each one individually for correctness.  Any errors found are
		added to the [errors] [DynArray].
	*)
	let check errors comm (perm_label, perm_order, perm_extra, perm_secondary) =

		(match reason_why_invalid perm_label comm.Ast.comm_label with
			| None ->
				()
			| Some reason ->
				let msg = Error.Bad_label_parameter (comm.Ast.comm_tag, reason) in
				DynArray.add errors (comm.Ast.comm_linenum, msg));

		(match reason_why_invalid perm_order comm.Ast.comm_order with
			| None ->
				()
			| Some reason ->
				let msg = Error.Bad_order_parameter (comm.Ast.comm_tag, reason) in
				DynArray.add errors (comm.Ast.comm_linenum, msg));

		(match reason_why_invalid perm_extra comm.Ast.comm_extra with
			| None ->
				()
			| Some reason ->
				let msg = Error.Bad_extra_parameter (comm.Ast.comm_tag, reason) in
				DynArray.add errors (comm.Ast.comm_linenum, msg));

		(match reason_why_invalid perm_secondary comm.Ast.comm_secondary with
			| None ->
				()
			| Some reason ->
				let msg = Error.Bad_secondary_parameter (comm.Ast.comm_tag, reason) in
				DynArray.add errors (comm.Ast.comm_linenum, msg))
end

