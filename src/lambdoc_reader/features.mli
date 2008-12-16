(********************************************************************************)
(*	Interface file for Features module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of document features.
*)


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
	| `Feature_sup | `Feature_sub | `Feature_mbox | `Feature_link
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
	[ `Feature_equation | `Feature_printout | `Feature_table | `Feature_figure 
	| `Feature_caption | `Feature_bib | `Feature_note
	| `Feature_bib_title | `Feature_bib_author | `Feature_bib_resource
	| `Feature_part | `Feature_appendix | `Feature_section1 | `Feature_section2 | `Feature_section3
	| `Feature_bibliography | `Feature_notes | `Feature_toc
	| `Feature_title1 | `Feature_title2 | `Feature_abstract | `Feature_rule
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


type t


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

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

