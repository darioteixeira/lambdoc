(********************************************************************************)
(*	Idiosyncrasies.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

module Feature_map =
	Map.Make (struct type t = Features.manuscript_feature_t let compare = Pervasives.compare end)


type t = bool Feature_map.t


(********************************************************************************)
(**	{2 Functions and values}						*)
(********************************************************************************)

let non_reference_inline_features =
	[
	`Feature_plain; `Feature_entity; `Feature_mathtex_inl; `Feature_mathml_inl;
	`Feature_bold; `Feature_emph; `Feature_mono; `Feature_caps; `Feature_thru;
	`Feature_sup; `Feature_sub; `Feature_mbox; `Feature_link;
	]

let reference_inline_features =
	[
	`Feature_see; `Feature_cite; `Feature_ref; `Feature_sref; `Feature_mref;
	]

let non_reference_block_features =
	[
	`Feature_item; `Feature_describe;
	`Feature_paragraph; `Feature_itemize; `Feature_enumerate; `Feature_description;
	`Feature_quote; `Feature_callout; `Feature_mathtex_blk; `Feature_mathml_blk;
	`Feature_code; `Feature_tabular; `Feature_verbatim; `Feature_bitmap; `Feature_subpage;
	]

let reference_block_features =
	[
	`Feature_equation; `Feature_printout; `Feature_table; `Feature_figure;
	`Feature_caption; `Feature_bib; `Feature_note;
	`Feature_bib_author; `Feature_bib_title; `Feature_bib_resource;
	`Feature_part; `Feature_appendix; `Feature_section1; `Feature_section2; `Feature_section3;
	`Feature_bibliography; `Feature_notes; `Feature_toc;
	`Feature_title1; `Feature_title2; `Feature_abstract; `Feature_rule;
	]


let composition_features =
	non_reference_inline_features @
	non_reference_block_features


let manuscript_features =
	composition_features @
	reference_inline_features @
	reference_block_features


let make_idiosyncrasies feature_set accept_list deny_list default =
	let default_bool = default = `Accept in
	let is_accepted feature =
		if List.mem feature deny_list
		then false
		else	 if List.mem feature accept_list
			then true
			else	if List.mem feature feature_set
				then default_bool
				else false in
	let make_feature map feature =
		Feature_map.add feature (is_accepted feature) map
	in List.fold_left make_feature Feature_map.empty manuscript_features


let make_composition_idiosyncrasies ?(accept_list = []) ?(deny_list = []) ?(default = `Accept) () =
	let composition_features = (composition_features :> Features.manuscript_feature_t list)
	and accept_list = (accept_list :> Features.manuscript_feature_t list)
	and deny_list = (deny_list :> Features.manuscript_feature_t list)
	in make_idiosyncrasies composition_features accept_list deny_list default


let make_manuscript_idiosyncrasies ?(accept_list = []) ?(deny_list = []) ?(default = `Accept) () =
	make_idiosyncrasies manuscript_features accept_list deny_list default


let check_feature feature map =
	Feature_map.find feature map

