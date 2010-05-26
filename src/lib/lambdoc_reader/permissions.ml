(********************************************************************************)
(*	Permissions.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core
open Basic
open Ast


(********************************************************************************)
(**	{1 Type definitions}							*)
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
(**	{1 Functions and values}						*)
(********************************************************************************)

(**	The following values/functions encode the predefined permissions for
	the various classes of commands.  Each permission class is a 3-tuple
	stating the individual permissions for the label, ordering, and extra
	parameters, respectively.  While most classes are constant, some of
	them are context-sensitive and are therefore functions.
*)

let forbidden_class =
	(Forbidden, Forbidden, Forbidden0)

let custom_heading_class minipaged =
	(Optional, (if minipaged then Mandatory0 else Forbidden0), Forbidden0)

let preset_heading_class =
	(Optional, Forbidden, Forbidden0)

let extra_class =
	(Forbidden, Forbidden, Optional0)

let custom_class =
	(*	Note that the order parameter is not actually "Optional0";
		because of its complexity it was checked by the caller,
		so we treat it as "Optional0" so errors are not triggered.
	*)
	(Optional, Optional0, Optional0)

let wrapper_class minipaged =
	(Optional, (if minipaged then Mandatory0 else Forbidden0), Optional0)

let ghost_class = (Optional, Forbidden, Forbidden0)


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
			let msg = Error.Misplaced_label_parameter (comm.comm_tag, reason) in
			DynArray.add errors (Some comm.comm_linenum, msg)

	and () = match reason_why_invalid perm_order comm.comm_order with
		| None ->
			()
		| Some reason ->
			let msg = Error.Misplaced_order_parameter (comm.comm_tag, reason) in
			DynArray.add errors (Some comm.comm_linenum, msg)

	and () = match reason_why_invalid perm_extra comm.comm_extra with
		| None ->
			()
		| Some reason ->
			let msg = Error.Misplaced_extra_parameter (comm.comm_tag, reason) in
			DynArray.add errors (Some comm.comm_linenum, msg)
	in ()


(**	Checks a command feature.
*)
let check_feature ?(maybe_minipaged=None) ?(maybe_wrapped=None) errors comm feature =

	let get_minipaged = function
		| Some minipaged	-> minipaged
		| None			-> invalid_arg "Feature requires that 'minipaged' be set but it is not!" in

	let composition_inline_feature_set = function
		| `Feature_plain	-> forbidden_class
		| `Feature_entity	-> forbidden_class
		| `Feature_linebreak	-> forbidden_class
		| `Feature_mathtex_inl	-> forbidden_class
		| `Feature_mathml_inl	-> forbidden_class
		| `Feature_glyph	-> forbidden_class
		| `Feature_bold		-> forbidden_class
		| `Feature_emph		-> forbidden_class
		| `Feature_code		-> forbidden_class
		| `Feature_caps		-> forbidden_class
		| `Feature_ins		-> forbidden_class
		| `Feature_del		-> forbidden_class
		| `Feature_sup		-> forbidden_class
		| `Feature_sub		-> forbidden_class
		| `Feature_mbox		-> forbidden_class
		| `Feature_span		-> extra_class
		| `Feature_link		-> forbidden_class

	and manuscript_inline_feature_set = function
		| `Feature_see		-> forbidden_class
		| `Feature_cite		-> forbidden_class
		| `Feature_ref		-> forbidden_class
		| `Feature_sref		-> forbidden_class
		| `Feature_mref		-> forbidden_class

	and composition_block_feature_set = function
		| `Feature_paragraph	-> extra_class
		| `Feature_itemize	-> extra_class
		| `Feature_enumerate	-> extra_class
		| `Feature_description	-> forbidden_class
		| `Feature_qanda	-> forbidden_class
		| `Feature_verse	-> forbidden_class
		| `Feature_quote	-> forbidden_class
		| `Feature_mathtex_blk	-> forbidden_class
		| `Feature_mathml_blk	-> forbidden_class
		| `Feature_source	-> extra_class
		| `Feature_tabular	-> forbidden_class
		| `Feature_console	-> forbidden_class
		| `Feature_verbatim	-> extra_class
		| `Feature_picture	-> extra_class
		| `Feature_subpage	-> forbidden_class

	and manuscript_block_feature_set = function
		| `Feature_decor	-> extra_class
		| `Feature_pullquote	-> extra_class

		| `Feature_equation	-> wrapper_class (get_minipaged maybe_minipaged)
		| `Feature_printout	-> wrapper_class (get_minipaged maybe_minipaged)
		| `Feature_table	-> wrapper_class (get_minipaged maybe_minipaged)
		| `Feature_figure	-> wrapper_class (get_minipaged maybe_minipaged)

		| `Feature_part		-> custom_heading_class (get_minipaged maybe_minipaged)
		| `Feature_appendix	-> preset_heading_class

		| `Feature_section1	-> custom_heading_class (get_minipaged maybe_minipaged)
		| `Feature_section2	-> custom_heading_class (get_minipaged maybe_minipaged)
		| `Feature_section3	-> custom_heading_class (get_minipaged maybe_minipaged)

		| `Feature_bibliography	-> preset_heading_class
		| `Feature_notes	-> preset_heading_class
		| `Feature_toc		-> preset_heading_class

		| `Feature_title1	-> forbidden_class
		| `Feature_title2	-> forbidden_class
		| `Feature_abstract	-> forbidden_class
		| `Feature_rule		-> forbidden_class

		| `Feature_bib		-> ghost_class
		| `Feature_note		-> ghost_class

		| `Feature_macrodef	-> forbidden_class
		| `Feature_boxoutdef	-> forbidden_class
		| `Feature_theoremdef	-> forbidden_class

	and internal_feature_set = function
		| `Feature_macrocall	-> forbidden_class
		| `Feature_macroarg	-> forbidden_class
		| `Feature_item		-> forbidden_class
		| `Feature_question	-> forbidden_class
		| `Feature_rquestion	-> forbidden_class
		| `Feature_answer	-> forbidden_class
		| `Feature_ranswer	-> forbidden_class
		| `Feature_thead	-> forbidden_class
		| `Feature_tbody	-> forbidden_class
		| `Feature_tfoot	-> forbidden_class
		| `Feature_bib_author	-> forbidden_class
		| `Feature_bib_title	-> forbidden_class
		| `Feature_bib_resource	-> forbidden_class
		| `Feature_custom	-> custom_class in

	let permission_set = match feature with
		| #Features.composition_inline_feature_t as x	-> composition_inline_feature_set x
		| #Features.manuscript_inline_feature_t as x	-> manuscript_inline_feature_set x
		| #Features.composition_block_feature_t as x	-> composition_block_feature_set x
		| #Features.manuscript_block_feature_t as x	-> manuscript_block_feature_set x
		| #Features.internal_feature_t as x		-> internal_feature_set x

	in check_permission_set errors comm permission_set

