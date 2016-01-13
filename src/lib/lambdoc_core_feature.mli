(********************************************************************************)
(*  Lambdoc_core_feature.mli
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Declarations concerning document features.
*)

open Lambdoc_core_basic


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type inline_feature =
    [ `Feature_plain | `Feature_entity | `Feature_linebreak
    | `Feature_mathtex_inl | `Feature_mathml_inl | `Feature_glyph
    | `Feature_bold | `Feature_emph | `Feature_code | `Feature_caps
    | `Feature_ins | `Feature_del | `Feature_sup | `Feature_sub
    | `Feature_mbox | `Feature_span | `Feature_link
    | `Feature_see | `Feature_cite | `Feature_dref | `Feature_sref | `Feature_mref
    | `Feature_extcomm_inl of ident ] [@@deriving sexp]

type block_feature =
    [ `Feature_paragraph 
    | `Feature_itemize | `Feature_enumerate | `Feature_description
    | `Feature_qanda | `Feature_verse | `Feature_quote
    | `Feature_mathtex_blk | `Feature_mathml_blk 
    | `Feature_source | `Feature_tabular 
    | `Feature_subpage | `Feature_verbatim | `Feature_picture | `Feature_pullquote
    | `Feature_equation | `Feature_printout | `Feature_table | `Feature_figure 
    | `Feature_part | `Feature_appendix
    | `Feature_section1 | `Feature_section2 | `Feature_section3 | `Feature_section4 | `Feature_section5 | `Feature_section6
    | `Feature_bibliography | `Feature_notes | `Feature_toc
    | `Feature_title1 | `Feature_title2
    | `Feature_abstract | `Feature_rule
    | `Feature_bib | `Feature_note
    | `Feature_macrodef | `Feature_boxoutdef | `Feature_theoremdef
    | `Feature_extcomm_blk of ident ] [@@deriving sexp]

type internal_feature =
    [ `Feature_item | `Feature_question | `Feature_rquestion | `Feature_answer | `Feature_ranswer
    | `Feature_cell | `Feature_thead | `Feature_tbody | `Feature_tfoot
    | `Feature_bib_author | `Feature_bib_title | `Feature_bib_resource
    | `Feature_custom | `Feature_macrocall | `Feature_macroarg ]

type public_feature = [ inline_feature | block_feature ] [@@deriving sexp]

type t = [ public_feature | internal_feature ] [@@deriving sexp]


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val inline_features: inline_feature list
val block_features: block_feature list
val internal_features: internal_feature list
val public_features: public_feature list
val features: t list
val describe: t -> string

