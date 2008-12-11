(********************************************************************************)
(*	Implementation file for Features module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

(********************************************************************************)
(**	{3 Auxiliary type definitions}						*)
(********************************************************************************)

type non_command_inline_feature_t =
	[ `Feature_plain | `Feature_entity | `Feature_mathtex_inl | `Feature_mathml_inl
	]

type non_reference_inline_feature_t =
	[ `Feature_bold | `Feature_emph | `Feature_mono | `Feature_caps | `Feature_thru
	| `Feature_sup | `Feature_sub | `Feature_box | `Feature_link
	]

type reference_inline_feature_t =
	[ `Feature_see | `Feature_cite | `Feature_ref | `Feature_sref | `Feature_mref
	]

type non_command_block_feature_t =
	[ `Feature_paragraph
	]

type non_reference_block_feature_t =
	[ `Feature_itemize | `Feature_enumerate | `Feature_quote | `Feature_mathtex_blk | `Feature_mathml_blk
	| `Feature_code | `Feature_verbatim | `Feature_tabular | `Feature_image | `Feature_subpage
	]

type reference_block_feature_t =
	[ `Feature_equation | `Feature_algorithm | `Feature_table | `Feature_figure 
	| `Feature_caption | `Feature_bib | `Feature_note
	| `Feature_bib_title | `Feature_bib_author | `Feature_bib_resource
	| `Feature_part | `Feature_appendix | `Feature_section1 | `Feature_section2 | `Feature_section3
	| `Feature_bibliography | `Feature_notes | `Feature_toc
	| `Feature_title1 | `Feature_title2 | `Feature_title3 | `Feature_abstract | `Feature_rule
	]


(********************************************************************************)
(**	{3 Main type definitions}						*)
(********************************************************************************)

type composition_feature_t =
	[ non_command_inline_feature_t 
	| non_reference_inline_feature_t 
	| non_command_block_feature_t 
	| non_reference_block_feature_t 
	]


type manuscript_feature_t =
	[ composition_feature_t 
	| reference_inline_feature_t 
	| reference_block_feature_t 
	]


type command_feature_t =
	[ non_reference_inline_feature_t 
	| non_reference_block_feature_t 
	| reference_inline_feature_t 
	| reference_block_feature_t 
	]


type default_t = [ `Accept | `Deny ]


module Feature_map =
	Map.Make (struct type t = manuscript_feature_t let compare = Pervasives.compare end)


type t = bool Feature_map.t


(********************************************************************************)
(**	{2 Functions and values}						*)
(********************************************************************************)

let non_command_inline_features =
	[
	`Feature_plain; `Feature_entity; `Feature_mathtex_inl; `Feature_mathml_inl;
	]

let non_reference_inline_features =
	[
	`Feature_bold; `Feature_emph; `Feature_mono; `Feature_caps; `Feature_thru;
	`Feature_sup; `Feature_sub; `Feature_box; `Feature_link;
	]

let reference_inline_features =
	[
	`Feature_see; `Feature_cite; `Feature_ref; `Feature_sref; `Feature_mref;
	]

let non_command_block_features =
	[
	`Feature_paragraph;
	]

let non_reference_block_features =
	[
	`Feature_itemize; `Feature_enumerate; `Feature_quote; `Feature_mathtex_blk; `Feature_mathml_blk;
	`Feature_code; `Feature_verbatim; `Feature_tabular; `Feature_image; `Feature_subpage;
	]

let reference_block_features =
	[
	`Feature_equation; `Feature_algorithm; `Feature_table; `Feature_figure;
	`Feature_caption; `Feature_bib; `Feature_note;
	`Feature_bib_title; `Feature_bib_author; `Feature_bib_resource;
	`Feature_part; `Feature_appendix; `Feature_section1; `Feature_section2; `Feature_section3;
	`Feature_bibliography; `Feature_notes; `Feature_toc;
	`Feature_title1; `Feature_title2; `Feature_title3; `Feature_abstract; `Feature_rule;
	]


let composition_features =
	non_command_inline_features @
	non_reference_inline_features @
	non_command_block_features @
	non_reference_block_features


let manuscript_features =
	composition_features @
	reference_inline_features @
	reference_block_features


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


let describe_non_command_inline_feature = function
	| `Feature_plain	-> ("plain", "plain text")
	| `Feature_entity	-> ("entity", "HTML entities")
	| `Feature_mathtex_inl	-> ("mathtex_inl", "inline TeX math")
	| `Feature_mathml_inl	-> ("mathml_inl", "inline MathML math")


let describe_non_reference_inline_feature = function
	| `Feature_bold		-> ("bold", "bold text")
	| `Feature_emph		-> ("emph", "emphasised text")
	| `Feature_mono		-> ("mono", "monospaced text")
	| `Feature_caps		-> ("caps", "small caps text")
	| `Feature_thru		-> ("thru", "strike-through text")
	| `Feature_sup		-> ("sup", "superscript text")
	| `Feature_sub		-> ("sub", "subscript text")
	| `Feature_box		-> ("box", "boxed text")
	| `Feature_link		-> ("link", "external link")


let describe_reference_inline_feature = function
	| `Feature_see		-> ("see", "reference to note")
	| `Feature_cite		-> ("cite", "bibliography citation")
	| `Feature_ref		-> ("ref", "internal reference")
	| `Feature_sref		-> ("sref", "smart reference")
	| `Feature_mref		-> ("mref", "manual reference")


let describe_non_command_block_feature = function
	| `Feature_paragraph	-> ("paragraph", "paragraph block")


let describe_non_reference_block_feature = function
	| `Feature_itemize	-> ("itemize", "itemize block")
	| `Feature_enumerate	-> ("enumerate", "enumerate block")
	| `Feature_quote	-> ("quote", "quote block")
	| `Feature_mathtex_blk	-> ("mathtex_blk", "TeX math block")
	| `Feature_mathml_blk	-> ("mathml_blk", "MathML block")
	| `Feature_code		-> ("code", "code block")
	| `Feature_verbatim	-> ("verbatim", "verbatim block")
	| `Feature_tabular	-> ("tabular", "tabular")
	| `Feature_image	-> ("image", "image block")
	| `Feature_subpage	-> ("subpage", "subpage block")


let describe_reference_block_feature = function
	| `Feature_equation	-> ("equation", "equation wrapper")
	| `Feature_algorithm	-> ("algorithm", "algorithm wrapper")
	| `Feature_table	-> ("table", "table wrapper")
	| `Feature_figure	-> ("figure", "figure wrapper")

	| `Feature_caption	-> ("caption", "wrapper caption")
	| `Feature_bib		-> ("bib", "bibliography entry")
	| `Feature_note		-> ("note", "note")

	| `Feature_bib_title	-> ("what", "title of biliography entry")
	| `Feature_bib_author	-> ("who", "author of bibliography entry")
	| `Feature_bib_resource	-> ("where", "resource of bibliography entry")

	| `Feature_part		-> ("part", "document part")
	| `Feature_appendix	-> ("appendix", "appendix")
	| `Feature_section1	-> ("section1", "section1")
	| `Feature_section2	-> ("section2", "section2")
	| `Feature_section3	-> ("section3", "section3")

	| `Feature_bibliography	-> ("bibliography", "bibliography")
	| `Feature_notes	-> ("notes", "notes")
	| `Feature_toc		-> ("toc", "table of contents")

	| `Feature_title1	-> ("title1", "document title1")
	| `Feature_title2	-> ("title2", "document title2")
	| `Feature_title3	-> ("title3", "document title3")
	| `Feature_abstract	-> ("abstract", "document abstract")
	| `Feature_rule		-> ("rule", "document rule")


let describe_feature = function
	| #non_command_inline_feature_t as x	-> describe_non_command_inline_feature x
	| #non_reference_inline_feature_t as x	-> describe_non_reference_inline_feature x
	| #reference_inline_feature_t as x	-> describe_reference_inline_feature x
	| #non_command_block_feature_t as x	-> describe_non_command_block_feature x
	| #non_reference_block_feature_t as x	-> describe_non_reference_block_feature x
	| #reference_block_feature_t as x	-> describe_reference_block_feature x

