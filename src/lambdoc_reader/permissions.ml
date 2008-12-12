(********************************************************************************)
(*	Implementation file for Permissions module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Basic
open Ast.M


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

(**	The type encoding the various kinds of available permissions.
*)
type permission_t =
	| Optional		(** The parameter is optional but may not be empty. *)
	| Optional0		(** The parameter is optional and may be empty. *)
	| Mandatory		(** The parameter is mandatory and may not be empty. *)
	| Mandatory0		(** The parameter is mandatory but may be empty. *)
	| Forbidden		(** The parameter is forbidden, either empty or not. *)
	| Forbidden0		(** The parameter is forbidden, unless it is empty. *)


(********************************************************************************)
(**	{2 Functions and values}						*)
(********************************************************************************)

(**	The following values/functions encode the predefined permissions for
	the various classes of commands.  Each permission class is a 4-tuple
	stating the individual permissions for the label, ordering, extra, and
	secondary parameters, respectively.  While most classes are constant,
	some of them are context-sensitive and are therefore functions.
*)

let forbidden_class =
	(Forbidden, Forbidden, Forbidden, Forbidden)

let custom_heading_class subpaged =
	let perm_order = if subpaged then Mandatory0 else Forbidden0
	in (Optional, perm_order, Forbidden, Forbidden)

let preset_heading_class =
	(Optional, Forbidden, Forbidden, Forbidden)

let listing_class =
	(Forbidden, Forbidden, Optional, Forbidden)

let quote_class =
	(Forbidden, Forbidden, Optional, Forbidden)

let floater_class =
	(Forbidden, Forbidden, Optional, Forbidden)

let code_class =
	(Forbidden, Forbidden, Optional, Mandatory)
	
let tabular_class =
	(Forbidden, Forbidden, Optional, Mandatory)

let wrapper_class subpaged =
	let perm_order = if subpaged then Mandatory else Forbidden
	in (Optional, perm_order, Optional, Forbidden)

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
let check_permission_set errors comm (perm_label, perm_order, perm_extra, perm_secondary) =

	(match reason_why_invalid perm_label comm.comm_label with
		| None ->
			()
		| Some reason ->
			let msg = Error.Bad_label_parameter (comm.comm_tag, reason) in
			DynArray.add errors (comm.comm_linenum, msg));

	(match reason_why_invalid perm_order comm.comm_order with
		| None ->
			()
		| Some reason ->
			let msg = Error.Bad_order_parameter (comm.comm_tag, reason) in
			DynArray.add errors (comm.comm_linenum, msg));

	(match reason_why_invalid perm_extra comm.comm_extra with
		| None ->
			()
		| Some reason ->
			let msg = Error.Bad_extra_parameter (comm.comm_tag, reason) in
			DynArray.add errors (comm.comm_linenum, msg));

	(match reason_why_invalid perm_secondary comm.comm_secondary with
		| None ->
			()
		| Some reason ->
			let msg = Error.Bad_secondary_parameter (comm.comm_tag, reason) in
			DynArray.add errors (comm.comm_linenum, msg))


(**	Checks a command feature.
*)
let check_command_feature errors comm maybe_subpaged feature =

	let get_subpaged = function
		| Some subpaged	-> subpaged
		| None		-> invalid_arg "Feature requires that 'subpaged' be set but it is not!" in

	let non_reference_inline_feature_set = function
		| `Feature_bold		-> forbidden_class
		| `Feature_emph		-> forbidden_class
		| `Feature_mono		-> forbidden_class
		| `Feature_caps		-> forbidden_class
		| `Feature_thru		-> forbidden_class
		| `Feature_sup		-> forbidden_class
		| `Feature_sub		-> forbidden_class
		| `Feature_box		-> forbidden_class
		| `Feature_link		-> forbidden_class

	and reference_inline_feature_set = function
		| `Feature_see		-> forbidden_class
		| `Feature_cite		-> forbidden_class
		| `Feature_ref		-> forbidden_class
		| `Feature_sref		-> forbidden_class
		| `Feature_mref		-> forbidden_class

	and non_reference_block_feature_set = function
		| `Feature_itemize	-> listing_class
		| `Feature_enumerate	-> listing_class
		| `Feature_quote	-> floater_class
		| `Feature_mathtex_blk	-> floater_class
		| `Feature_mathml_blk	-> floater_class
		| `Feature_code		-> code_class
		| `Feature_verbatim	-> floater_class
		| `Feature_tabular	-> tabular_class
		| `Feature_image	-> floater_class
		| `Feature_subpage	-> floater_class

	and reference_block_feature_set = function
		| `Feature_equation	-> wrapper_class (get_subpaged maybe_subpaged)
		| `Feature_algorithm	-> wrapper_class (get_subpaged maybe_subpaged)
		| `Feature_table	-> wrapper_class (get_subpaged maybe_subpaged)
		| `Feature_figure	-> wrapper_class (get_subpaged maybe_subpaged)

		| `Feature_caption	-> forbidden_class
		| `Feature_bib		-> ghost_class
		| `Feature_note		-> ghost_class

		| `Feature_bib_title	-> forbidden_class
		| `Feature_bib_author	-> forbidden_class
		| `Feature_bib_resource	-> forbidden_class

		| `Feature_part		-> custom_heading_class (get_subpaged maybe_subpaged)
		| `Feature_appendix	-> forbidden_class
		| `Feature_section1	-> custom_heading_class (get_subpaged maybe_subpaged)
		| `Feature_section2	-> custom_heading_class (get_subpaged maybe_subpaged)
		| `Feature_section3	-> custom_heading_class (get_subpaged maybe_subpaged)

		| `Feature_bibliography	-> preset_heading_class
		| `Feature_notes	-> preset_heading_class
		| `Feature_toc		-> preset_heading_class

		| `Feature_title1	-> forbidden_class
		| `Feature_title2	-> forbidden_class
		| `Feature_abstract	-> forbidden_class
		| `Feature_rule		-> forbidden_class in

	let permission_set = match feature with
		| #Features.non_reference_inline_feature_t as x	-> non_reference_inline_feature_set x
		| #Features.reference_inline_feature_t as x	-> reference_inline_feature_set x
		| #Features.non_reference_block_feature_t as x	-> non_reference_block_feature_set x
		| #Features.reference_block_feature_t as x	-> reference_block_feature_set x

	in check_permission_set errors comm permission_set

