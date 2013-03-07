(********************************************************************************)
(*	Permissions.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core
open Idiosyncrasies
open Ast


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

(**	The type encoding the various kinds of available permissions.
*)
type permission_t =
	| Optional		(** The parameter is optional but may not be empty. *)
	| Optional0		(** The parameter is optional and may be empty. *)
	| Mandatory0		(** The parameter is mandatory but may be empty. *)
	| Forbidden		(** The parameter is forbidden, either empty or not. *)
	| Forbidden0		(** The parameter is forbidden, unless it is empty. *)


(********************************************************************************)
(**	{1 Private functions and values}					*)
(********************************************************************************)

let forbidden_class = (Forbidden, Forbidden)


let custom_heading_class minipaged = (Optional, if minipaged then Mandatory0 else Forbidden0)


let preset_heading_class = (Optional, Forbidden)


let custom_class = (Optional, Optional0)
	(* Note that the order parameter is not actually "Optional0"; because of its complexity
	   it was checked by the caller, so we treat it as "Optional0" so errors are not triggered. *)


let wrapper_class minipaged = (Optional, if minipaged then Mandatory0 else Forbidden0)


let ghost_class = (Optional, Forbidden)


let reason_why_invalid perm = function
	| Some "" ->
		begin match perm with
			| Optional0
			| Mandatory0
			| Forbidden0 -> None
			| Optional   -> Some Error.Reason_is_empty_when_non_empty_mandatory
			| Forbidden  -> Some Error.Reason_is_empty_when_forbidden
		end
	| Some other ->
		begin match perm with
			| Forbidden
			| Forbidden0 -> Some (Error.Reason_is_non_empty_when_forbidden other)
			| _	     -> None
		end
	| None ->
		begin match perm with
			| Mandatory0 -> Some Error.Reason_is_absent_when_mandatory
			| _	     -> None
		end


let check_permission_set errors comm (perm_label, perm_order) =
	begin match reason_why_invalid perm_label comm.comm_label with
		| None ->
			()
		| Some reason ->
			let msg = Error.Misplaced_label_parameter (comm.comm_tag, reason) in
			BatDynArray.add errors (Some comm.comm_linenum, msg)
	end;
	begin match reason_why_invalid perm_order comm.comm_order with
		| None ->
			()
		| Some reason ->
			let msg = Error.Misplaced_order_parameter (comm.comm_tag, reason) in
			BatDynArray.add errors (Some comm.comm_linenum, msg)
	end


let classify = function
	| (_, `Any)		-> true
	| (x, `Only target)	-> (x = target)
	| (x, `Member targets)	-> List.mem x targets


let run_ruleset verify ruleset default =
	let rec iterate = function
		| []	   -> default
		| (rule, action) :: tl -> if verify rule then action else iterate tl
	in match iterate ruleset with
		| `Accept -> true
		| `Deny	  -> false


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

let check_parameters ?(maybe_minipaged = None) ?(maybe_wrapped = None) errors comm feature =

	let get_minipaged = function
		| Some minipaged	-> minipaged
		| None			-> invalid_arg "Feature requires that 'minipaged' be set but it is not!" in

	let inline_feature_set = function
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
		| `Feature_span		-> forbidden_class
		| `Feature_link		-> forbidden_class
		| `Feature_booklink	-> forbidden_class
		| `Feature_see		-> forbidden_class
		| `Feature_cite		-> forbidden_class
		| `Feature_ref		-> forbidden_class
		| `Feature_sref		-> forbidden_class

	and block_feature_set = function
		| `Feature_paragraph	-> forbidden_class
		| `Feature_itemize	-> forbidden_class
		| `Feature_enumerate	-> forbidden_class
		| `Feature_description	-> forbidden_class
		| `Feature_qanda	-> forbidden_class
		| `Feature_verse	-> forbidden_class
		| `Feature_quote	-> forbidden_class
		| `Feature_mathtex_blk	-> forbidden_class
		| `Feature_mathml_blk	-> forbidden_class
		| `Feature_source	-> forbidden_class
		| `Feature_tabular	-> forbidden_class
		| `Feature_subpage	-> forbidden_class
		| `Feature_verbatim	-> forbidden_class
		| `Feature_picture	-> forbidden_class
		| `Feature_bookpic	-> forbidden_class
		| `Feature_pullquote	-> forbidden_class
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
		| #Feature.inline_feature_t as x   -> inline_feature_set x
		| #Feature.block_feature_t as x    -> block_feature_set x
		| #Feature.internal_feature_t as x -> internal_feature_set x

	in check_permission_set errors comm permission_set


let check_feature feature {feature_ruleset; feature_default; _} =
	let verify rule =
		classify (feature, rule) in
	run_ruleset verify feature_ruleset feature_default


let check_classname feature classname {classname_ruleset; classname_default; _} =
	let verify (rule1, rule2) =
		classify (feature, rule1) && classify (classname, rule2) in
	run_ruleset verify classname_ruleset classname_default

