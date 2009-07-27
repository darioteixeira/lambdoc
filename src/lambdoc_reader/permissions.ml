(********************************************************************************)
(*	Permissions.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core
open Basic
open Ast


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
	the various classes of commands.  Each permission class is a 3-tuple
	stating the individual permissions for the label, ordering, and extra
	parameters, respectively.  While most classes are constant, some of
	them are context-sensitive and are therefore functions.
*)

let forbidden_class =
	(Forbidden, Forbidden, Forbidden)

let custom_heading_class subpaged =
	(Optional, (if subpaged then Mandatory0 else Forbidden0), Forbidden)

let preset_heading_class =
	(Optional, Forbidden, Forbidden)

let listing_class =
	(Forbidden, Forbidden, Optional)

let floater_class =
	(Forbidden, Forbidden, Optional)

let wrapper_class subpaged =
	(Optional, (if subpaged then Mandatory else Forbidden), Optional)

let ghost_class = (Optional, Forbidden, Forbidden)


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
let check_permission_set errors comm (perm_label, perm_order, perm_extra) =

	let () = match reason_why_invalid perm_label comm.comm_label with
		| None ->
			()
		| Some reason ->
			let msg = Error.Bad_label_parameter (comm.comm_tag, reason) in
			DynArray.add errors (comm.comm_linenum, msg)
	and () = match reason_why_invalid perm_order comm.comm_order with
		| None ->
			()
		| Some reason ->
			let msg = Error.Bad_order_parameter (comm.comm_tag, reason) in
			DynArray.add errors (comm.comm_linenum, msg)
	and () = match reason_why_invalid perm_extra comm.comm_extra with
		| None ->
			()
		| Some reason ->
			let msg = Error.Bad_extra_parameter (comm.comm_tag, reason) in
			DynArray.add errors (comm.comm_linenum, msg)
	in ()


(**	Checks a command feature.
*)
let check_feature ?(maybe_subpaged=None) ?(maybe_wrapped=None) errors comm feature =

	let get_subpaged = function
		| Some subpaged	-> subpaged
		| None		-> invalid_arg "Feature requires that 'subpaged' be set but it is not!" in

	let non_reference_inline_feature_set = function
		| `Feature_plain	-> forbidden_class
		| `Feature_entity	-> forbidden_class
		| `Feature_break	-> forbidden_class
		| `Feature_mathtex_inl	-> forbidden_class
		| `Feature_mathml_inl	-> forbidden_class
		| `Feature_bold		-> forbidden_class
		| `Feature_emph		-> forbidden_class
		| `Feature_mono		-> forbidden_class
		| `Feature_caps		-> forbidden_class
		| `Feature_thru		-> forbidden_class
		| `Feature_sup		-> forbidden_class
		| `Feature_sub		-> forbidden_class
		| `Feature_mbox		-> forbidden_class
		| `Feature_link		-> forbidden_class

	and reference_inline_feature_set = function
		| `Feature_see		-> forbidden_class
		| `Feature_cite		-> forbidden_class
		| `Feature_ref		-> forbidden_class
		| `Feature_sref		-> forbidden_class
		| `Feature_mref		-> forbidden_class

	and non_reference_block_feature_set = function
		| `Feature_item		-> forbidden_class
		| `Feature_describe	-> forbidden_class
		| `Feature_paragraph	-> forbidden_class
		| `Feature_itemize	-> listing_class
		| `Feature_enumerate	-> listing_class
		| `Feature_description	-> forbidden_class
		| `Feature_quote	-> forbidden_class
		| `Feature_pullquote	-> floater_class
		| `Feature_boxout	-> floater_class
		| `Feature_mathtex_blk	-> floater_class
		| `Feature_mathml_blk	-> floater_class
		| `Feature_code		-> floater_class
		| `Feature_tabular	-> floater_class
		| `Feature_verbatim	-> floater_class
		| `Feature_bitmap	-> floater_class
		| `Feature_subpage	-> floater_class

	and reference_block_feature_set = function
		| `Feature_equation	-> wrapper_class (get_subpaged maybe_subpaged)
		| `Feature_printout	-> wrapper_class (get_subpaged maybe_subpaged)
		| `Feature_table	-> wrapper_class (get_subpaged maybe_subpaged)
		| `Feature_figure	-> wrapper_class (get_subpaged maybe_subpaged)

		| `Feature_caption	-> forbidden_class
		| `Feature_bib		-> ghost_class
		| `Feature_note		-> ghost_class

		| `Feature_bib_author	-> forbidden_class
		| `Feature_bib_title	-> forbidden_class
		| `Feature_bib_resource	-> forbidden_class

		| `Feature_part		-> custom_heading_class (get_subpaged maybe_subpaged)
		| `Feature_appendix	-> preset_heading_class
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

