(********************************************************************************)
(*	Implementation file for Document_features.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of document features.
*)

(********************************************************************************)
(*	{2 Features module}							*)
(********************************************************************************)

module Features:
sig
	type t

	type default_t = [ `Accept | `Deny ]

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
		| `Feature_bibliography | `Feature_notes | `Feature_title | `Feature_subtitle | `Feature_abstract
		| `Feature_rule | `Feature_appendix
		]

	type command_feature_t =
		[ `Feature_bold | `Feature_emph | `Feature_mono | `Feature_caps | `Feature_thru | `Feature_sup
		| `Feature_sub | `Feature_box | `Feature_link | `Feature_itemize | `Feature_enumerate | `Feature_quote
		| `Feature_mathtex_blk | `Feature_mathml_blk | `Feature_code | `Feature_verbatim | `Feature_tabular
		| `Feature_image | `Feature_subpage | `Feature_see | `Feature_cite | `Feature_ref | `Feature_sref
		| `Feature_mref | `Feature_caption | `Feature_bib_title | `Feature_bib_author | `Feature_bib_resource
		| `Feature_equation | `Feature_algorithm | `Feature_table | `Feature_figure | `Feature_bib
		| `Feature_note | `Feature_section | `Feature_subsection | `Feature_subsubsection | `Feature_toc
		| `Feature_bibliography | `Feature_notes | `Feature_title | `Feature_subtitle | `Feature_abstract
		| `Feature_rule | `Feature_appendix
		]

	val load_composition_features:
		?deny_list: composition_feature_t list ->
		?accept_list: composition_feature_t list ->
		?default: default_t ->
		unit -> t

	val load_manuscript_features:
		?deny_list: manuscript_feature_t list ->
		?accept_list: manuscript_feature_t list ->
		?default: default_t ->
		unit -> t

	val check_feature: manuscript_feature_t -> t -> bool

	val describe_feature: manuscript_feature_t -> string * string
end =
struct
	type default_t = [ `Accept | `Deny ]

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
		| `Feature_bibliography | `Feature_notes | `Feature_title | `Feature_subtitle | `Feature_abstract
		| `Feature_rule | `Feature_appendix
		]

	type command_feature_t =
		[ `Feature_bold | `Feature_emph | `Feature_mono | `Feature_caps | `Feature_thru | `Feature_sup
		| `Feature_sub | `Feature_box | `Feature_link | `Feature_itemize | `Feature_enumerate | `Feature_quote
		| `Feature_mathtex_blk | `Feature_mathml_blk | `Feature_code | `Feature_verbatim | `Feature_tabular
		| `Feature_image | `Feature_subpage | `Feature_see | `Feature_cite | `Feature_ref | `Feature_sref
		| `Feature_mref | `Feature_caption | `Feature_bib_title | `Feature_bib_author | `Feature_bib_resource
		| `Feature_equation | `Feature_algorithm | `Feature_table | `Feature_figure | `Feature_bib
		| `Feature_note | `Feature_section | `Feature_subsection | `Feature_subsubsection | `Feature_toc
		| `Feature_bibliography | `Feature_notes | `Feature_title | `Feature_subtitle | `Feature_abstract
		| `Feature_rule | `Feature_appendix
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
		`Feature_bibliography; `Feature_notes; `Feature_title; `Feature_subtitle; `Feature_abstract;
		`Feature_rule; `Feature_appendix
		]

	let load_features feature_set deny_list accept_list default =
		let default_bool = default = `Accept in
		let is_accepted feature =
			if List.mem feature deny_list
			then false
			else	 if List.mem feature accept_list
				then true
				else	if List.mem feature feature_set
					then default_bool
					else false in
		let load_feature map feature =
			Feature_map.add feature (is_accepted feature) map
		in List.fold_left load_feature Feature_map.empty manuscript_features

	let load_composition_features ?(deny_list = []) ?(accept_list = []) ?(default = `Accept) () =
		let composition_features = (composition_features :> manuscript_feature_t list)
		and deny_list = (deny_list :> manuscript_feature_t list)
		and accept_list = (accept_list :> manuscript_feature_t list)
		in load_features composition_features deny_list accept_list default

	let load_manuscript_features ?(deny_list = []) ?(accept_list = []) ?(default = `Accept) () =
		load_features manuscript_features deny_list accept_list default

	let check_feature feature map =
		Feature_map.find feature map

	let describe_feature = function
		| `Feature_plain		-> ("plain", "plain text")
		| `Feature_entity		-> ("entity", "HTML entities")
		| `Feature_mathtex_inl		-> ("mathtex_inl", "inline TeX math")
		| `Feature_mathml_inl		-> ("mathml_inl", "inline MathML math")
		| `Feature_bold			-> ("bold", "bold text")
		| `Feature_emph			-> ("emph", "emphasised text")
		| `Feature_mono			-> ("mono", "monospaced text")
		| `Feature_caps			-> ("caps", "small caps text")
		| `Feature_thru			-> ("thru", "strike-through text")
		| `Feature_sup			-> ("sup", "superscript text")
		| `Feature_sub			-> ("sub", "subscript text")
		| `Feature_box			-> ("box", "boxed text")
		| `Feature_link			-> ("link", "external link")
		| `Feature_paragraph		-> ("paragraph", "paragraph block")
		| `Feature_itemize		-> ("itemize", "itemize block")
		| `Feature_enumerate		-> ("enumerate", "enumerate block")
		| `Feature_quote		-> ("quote", "quote block")
		| `Feature_mathtex_blk		-> ("mathtex_blk", "TeX math block")
		| `Feature_mathml_blk		-> ("mathml_blk", "MathML block")
		| `Feature_code			-> ("code", "code block")
		| `Feature_verbatim		-> ("verbatim", "verbatim block")
		| `Feature_tabular		-> ("tabular", "tabular")
		| `Feature_image		-> ("image", "image block")
		| `Feature_subpage		-> ("subpage", "subpage block")
		| `Feature_see			-> ("see", "reference to note")
		| `Feature_cite			-> ("cite", "bibliography citation")
		| `Feature_ref			-> ("ref", "internal reference")
		| `Feature_sref			-> ("sref", "smart reference")
		| `Feature_mref			-> ("mref", "manual reference")
		| `Feature_caption		-> ("caption", "wrapper caption")
		| `Feature_bib_title		-> ("what", "title of biliography entry")
		| `Feature_bib_author		-> ("who", "author of bibliography entry")
		| `Feature_bib_resource		-> ("where", "resource of bibliography entry")
		| `Feature_equation		-> ("equation", "equation wrapper")
		| `Feature_algorithm		-> ("algorithm", "algorithm wrapper")
		| `Feature_table		-> ("table", "table wrapper")
		| `Feature_figure		-> ("figure", "figure wrapper")
		| `Feature_bib			-> ("bib", "bibliography entry")
		| `Feature_note			-> ("note", "note")
		| `Feature_section		-> ("section", "section")
		| `Feature_subsection		-> ("subsection", "sub-section")
		| `Feature_subsubsection	-> ("subsubsection", "sub-sub-section")
		| `Feature_toc			-> ("toc", "table of contents")
		| `Feature_bibliography		-> ("bibliography", "bibliography")
		| `Feature_notes		-> ("notes", "notes")
		| `Feature_title		-> ("title", "document title")
		| `Feature_subtitle		-> ("subtitle", "document sub-title")
		| `Feature_abstract		-> ("abstract", "document abstract")
		| `Feature_rule			-> ("rule", "document rule")
		| `Feature_appendix		-> ("appendix", "appendix")
end

