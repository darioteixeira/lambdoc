(********************************************************************************)
(*	Features.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
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


(********************************************************************************)
(**	{2 Public values and functions}						*)
(********************************************************************************)

val available_composition_features: composition_feature_t list
val available_manuscript_features: manuscript_feature_t list

val describe: manuscript_feature_t -> string

