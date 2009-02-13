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

type non_reference_inline_feature_t =
	[ `Feature_plain | `Feature_entity | `Feature_mathtex_inl | `Feature_mathml_inl
	| `Feature_bold | `Feature_emph | `Feature_mono | `Feature_caps | `Feature_thru
	| `Feature_sup | `Feature_sub | `Feature_mbox | `Feature_link
	]

type reference_inline_feature_t =
	[ `Feature_see | `Feature_cite | `Feature_ref | `Feature_sref | `Feature_mref
	]

type non_reference_block_feature_t =
	[ `Feature_item | `Feature_describe
	| `Feature_paragraph | `Feature_itemize | `Feature_enumerate | `Feature_description
	| `Feature_quote | `Feature_callout | `Feature_mathtex_blk | `Feature_mathml_blk
	| `Feature_code | `Feature_tabular | `Feature_verbatim | `Feature_bitmap | `Feature_subpage
	]

type reference_block_feature_t =
	[ `Feature_equation | `Feature_printout | `Feature_table | `Feature_figure 
	| `Feature_caption | `Feature_bib | `Feature_note
	| `Feature_bib_author | `Feature_bib_title | `Feature_bib_resource
	| `Feature_part | `Feature_appendix | `Feature_section1 | `Feature_section2 | `Feature_section3
	| `Feature_bibliography | `Feature_notes | `Feature_toc
	| `Feature_title1 | `Feature_title2 | `Feature_abstract | `Feature_rule
	]


(********************************************************************************)
(**	{3 Main type definitions}						*)
(********************************************************************************)

type composition_feature_t =
	[ non_reference_inline_feature_t 
	| non_reference_block_feature_t 
	]


type manuscript_feature_t =
	[ composition_feature_t 
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


let load_features feature_set accept_list deny_list default =
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


let load_composition_features ?(accept_list = []) ?(deny_list = []) ?(default = `Accept) () =
	let composition_features = (composition_features :> manuscript_feature_t list)
	and accept_list = (accept_list :> manuscript_feature_t list)
	and deny_list = (deny_list :> manuscript_feature_t list)
	in load_features composition_features accept_list deny_list default


let load_manuscript_features ?(accept_list = []) ?(deny_list = []) ?(default = `Accept) () =
	load_features manuscript_features accept_list deny_list default


let check_feature feature map =
	Feature_map.find feature map


let describe_non_reference_inline_feature = function
	| `Feature_plain	-> "plain text"
	| `Feature_entity	-> "HTML entities"
	| `Feature_mathtex_inl	-> "inline TeX math"
	| `Feature_mathml_inl	-> "inline MathML math"
	| `Feature_bold		-> "bold text"
	| `Feature_emph		-> "emphasised text"
	| `Feature_mono		-> "monospaced text"
	| `Feature_caps		-> "small caps text"
	| `Feature_thru		-> "strike-through text"
	| `Feature_sup		-> "superscript text"
	| `Feature_sub		-> "subscript text"
	| `Feature_mbox		-> "boxed text"
	| `Feature_link		-> "external link"


let describe_reference_inline_feature = function
	| `Feature_see		-> "reference to note"
	| `Feature_cite		-> "bibliography citation"
	| `Feature_ref		-> "internal reference"
	| `Feature_sref		-> "smart reference"
	| `Feature_mref		-> "manual reference"


let describe_non_reference_block_feature = function
	| `Feature_item		-> "listing item"
	| `Feature_describe	-> "description item"
	| `Feature_paragraph	-> "paragraph block"
	| `Feature_itemize	-> "itemize block"
	| `Feature_enumerate	-> "enumerate block"
	| `Feature_description	-> "description block"
	| `Feature_quote	-> "quote block"
	| `Feature_callout	-> "callout block"
	| `Feature_mathtex_blk	-> "TeX math block"
	| `Feature_mathml_blk	-> "MathML block"
	| `Feature_code		-> "code block"
	| `Feature_tabular	-> "tabular"
	| `Feature_verbatim	-> "verbatim block"
	| `Feature_bitmap	-> "bitmap block"
	| `Feature_subpage	-> "subpage block"


let describe_reference_block_feature = function
	| `Feature_equation	-> "equation wrapper"
	| `Feature_printout	-> "printout wrapper"
	| `Feature_table	-> "table wrapper"
	| `Feature_figure	-> "figure wrapper"

	| `Feature_caption	-> "wrapper caption"
	| `Feature_bib		-> "bibliography entry"
	| `Feature_note		-> "note"

	| `Feature_bib_author	-> "author of bibliography entry"
	| `Feature_bib_title	-> "title of biliography entry"
	| `Feature_bib_resource	-> "resource of bibliography entry"

	| `Feature_part		-> "document part"
	| `Feature_appendix	-> "appendix"
	| `Feature_section1	-> "section1"
	| `Feature_section2	-> "section2"
	| `Feature_section3	-> "section3"

	| `Feature_bibliography	-> "bibliography"
	| `Feature_notes	-> "notes"
	| `Feature_toc		-> "table of contents"

	| `Feature_title1	-> "document title1"
	| `Feature_title2	-> "document title2"
	| `Feature_abstract	-> "document abstract"
	| `Feature_rule		-> "document rule"


let describe_feature = function
	| #non_reference_inline_feature_t as x	-> describe_non_reference_inline_feature x
	| #reference_inline_feature_t as x	-> describe_reference_inline_feature x
	| #non_reference_block_feature_t as x	-> describe_non_reference_block_feature x
	| #reference_block_feature_t as x	-> describe_reference_block_feature x

