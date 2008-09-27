(********************************************************************************)
(**	Definition of document features.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)


(********************************************************************************)
(********************************************************************************)

module Features:
sig
	type t

	type composition_feature_t =
		[ `Feature_plain | `Feature_entity | `Feature_mathtex_inl | `Feature_mathml_inl | `Feature_bold
		| `Feature_emph | `Feature_mono | `Feature_caps | `Feature_thru | `Feature_sup | `Feature_sub
		| `Feature_box | `Feature_link | `Feature_paragraph | `Feature_itemize | `Feature_enumerate
		| `Feature_quote | `Feature_mathtex_blk | `Feature_mathml_blk | `Feature_code | `Feature_verbatim
		| `Feature_tabular | `Feature_image | `Feature_subpage
		]

	type manuscript_feature_t =
		[ composition_feature_t
		| `Feature_see | `Feature_cite | `Feature_ref | `Feature_sref | `Feature_mref | `Feature_caption
		| `Feature_bib_title | `Feature_bib_author | `Feature_bib_resource | `Feature_equation
		| `Feature_algorithm | `Feature_table | `Feature_figure | `Feature_bib | `Feature_note
		| `Feature_section | `Feature_subsection | `Feature_subsubsection | `Feature_toc
		| `Feature_bibliography | `Feature_notes | `Feature_title | `Feature_abstract
		| `Feature_rule | `Feature_appendix | `Feature_setting
		]

	val load_composition_features:
		?deny_list: composition_feature_t list ->
		?accept_list: composition_feature_t list ->
		?default: bool ->
		unit -> t

	val load_manuscript_features:
		?deny_list: manuscript_feature_t list ->
		?accept_list: manuscript_feature_t list ->
		?default: bool ->
		unit -> t

	val check_feature: manuscript_feature_t -> t -> bool
end =
struct
	type composition_feature_t =
		[ `Feature_plain | `Feature_entity | `Feature_mathtex_inl | `Feature_mathml_inl | `Feature_bold
		| `Feature_emph | `Feature_mono | `Feature_caps | `Feature_thru | `Feature_sup | `Feature_sub
		| `Feature_box | `Feature_link | `Feature_paragraph | `Feature_itemize | `Feature_enumerate
		| `Feature_quote | `Feature_mathtex_blk | `Feature_mathml_blk | `Feature_code | `Feature_verbatim
		| `Feature_tabular | `Feature_image | `Feature_subpage
		]

	type manuscript_feature_t =
		[ composition_feature_t
		| `Feature_see | `Feature_cite | `Feature_ref | `Feature_sref | `Feature_mref | `Feature_caption
		| `Feature_bib_title | `Feature_bib_author | `Feature_bib_resource | `Feature_equation
		| `Feature_algorithm | `Feature_table | `Feature_figure | `Feature_bib | `Feature_note
		| `Feature_section | `Feature_subsection | `Feature_subsubsection | `Feature_toc
		| `Feature_bibliography | `Feature_notes | `Feature_title | `Feature_abstract
		| `Feature_rule | `Feature_appendix | `Feature_setting
		]

	module Feature_map =
		Map.Make (struct type t = manuscript_feature_t let compare = Pervasives.compare end)

	type t = bool Feature_map.t

	let composition_features =
		[
		`Feature_plain; `Feature_entity; `Feature_mathtex_inl; `Feature_mathml_inl; `Feature_bold;
		`Feature_emph; `Feature_mono; `Feature_caps; `Feature_thru; `Feature_sup; `Feature_sub;
		`Feature_box; `Feature_link; `Feature_paragraph; `Feature_itemize; `Feature_enumerate;
		`Feature_quote; `Feature_mathtex_blk; `Feature_mathml_blk; `Feature_code; `Feature_verbatim;
		`Feature_tabular; `Feature_image; `Feature_subpage
		]

	let manuscript_features =
		composition_features @
		[
		`Feature_see; `Feature_cite; `Feature_ref; `Feature_sref; `Feature_mref; `Feature_caption;
		`Feature_bib_title; `Feature_bib_author; `Feature_bib_resource; `Feature_equation;
		`Feature_algorithm; `Feature_table; `Feature_figure; `Feature_bib; `Feature_note;
		`Feature_section; `Feature_subsection; `Feature_subsubsection; `Feature_toc;
		`Feature_bibliography; `Feature_notes; `Feature_title; `Feature_abstract;
		`Feature_rule; `Feature_appendix; `Feature_setting
		]

	let load_features feature_set deny_list accept_list default =
		let is_accepted feature =
			if List.mem feature deny_list
			then false
			else	 if List.mem feature accept_list
				then true
				else	if List.mem feature feature_set
					then default
					else false in
		let load_feature map feature =
			Feature_map.add feature (is_accepted feature) map
		in List.fold_left load_feature Feature_map.empty manuscript_features

	let load_composition_features ?(deny_list = []) ?(accept_list = []) ?(default = true) () =
		let composition_features = (composition_features :> manuscript_feature_t list)
		and deny_list = (deny_list :> manuscript_feature_t list)
		and accept_list = (accept_list :> manuscript_feature_t list)
		in load_features composition_features deny_list accept_list default

	let load_manuscript_features ?(deny_list = []) ?(accept_list = []) ?(default = true) () =
		load_features manuscript_features deny_list accept_list default

	let check_feature feature map =
		Feature_map.find feature map
end

