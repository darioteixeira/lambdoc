(********************************************************************************)
(*  Lambdoc_reader_ast.mli
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Definition of the document AST that all parsers are supposed to generate.
*)

open Lambdoc_core
open Basic


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

(********************************************************************************)
(** {2 Parsing data associated with command tags}                               *)
(********************************************************************************)

type command_t =
    {
    comm_tag: Ident.t option;
    comm_label: Pointer.t option;
    comm_order: string option;
    comm_style: string option;
    comm_linenum: int;
    }


(********************************************************************************)
(** {2 Data types for inline context}                                           *)
(********************************************************************************)

type seq_t = inline_t list
 and inline_t = command_t * raw_inline_t
 and raw_inline_t =
    | Plain of string                           (* Inlpat_raw *)
    | Entity of string                          (* Inlpat_raw *)
    | Linebreak                                 (* Inlpat_empty *)
    | Mathtex_inl of string                     (* Inlpat_raw *)
    | Mathml_inl of string                      (* Inlpat_raw *)
    | Glyph of string * string                  (* Inlpat_raw_raw *)
    | Bold of seq_t                             (* Inlpat_seq *)
    | Emph of seq_t                             (* Inlpat_seq *)
    | Code of seq_t                             (* Inlpat_seq *)
    | Caps of seq_t                             (* Inlpat_seq *)
    | Ins of seq_t                              (* Inlpat_seq *)
    | Del of seq_t                              (* Inlpat_seq *)
    | Sup of seq_t                              (* Inlpat_seq *)
    | Sub of seq_t                              (* Inlpat_seq *)
    | Mbox of seq_t                             (* Inlpat_seq *)
    | Span of seq_t                             (* Inlpat_seq *)
    | Link of string * seq_t option             (* Inlpat_raw_seqopt *)
    | See of string list                        (* Inlpat_rawlist *)
    | Cite of string list                       (* Inlpat_rawlist *)
    | Dref of string * seq_t option             (* Inlpat_raw_seqopt *)
    | Sref of string * seq_t option             (* Inlpat_raw_seqopt *)
    | Mref of string * seq_t                    (* Inlpat_raw_seq *)
    | Macroarg of string                        (* Inlpat_raw *)
    | Macrocall of string * seq_t list          (* Inlpat_raw_seqlist *)
    | Extcomm_inl of Ident.t * inline_pattern_t

and inline_pattern_t =
    | Inlpat_empty
    | Inlpat_seq of seq_t
    | Inlpat_raw of string
    | Inlpat_raw_raw of string * string
    | Inlpat_raw_seq of string * seq_t
    | Inlpat_raw_seqopt of string * seq_t option


(********************************************************************************)
(** {2 Data types for document blocks}                                          *)
(********************************************************************************)

type tabular_cell_t = command_t * string option * seq_t option

type tabular_row_t = command_t * tabular_cell_t list

type tabular_group_t = command_t * tabular_row_t list

type tabular_t =
    {
    thead: tabular_group_t option;
    tfoot: tabular_group_t option;
    tbodies: tabular_group_t list;
    }

type bib_t =
    {
    author: command_t * seq_t;
    title: command_t * seq_t;
    resource: command_t * seq_t;
    }

type frag_t = block_t list
 and block_t = command_t * raw_block_t
 and raw_block_t =
    | Paragraph of seq_t                                                (* Blkpat_seq *)
    | Itemize of (command_t * frag_t) list
    | Enumerate of (command_t * frag_t) list
    | Description of (command_t * seq_t * frag_t) list
    | Qanda of (command_t * qanda_t * frag_t) list
    | Verse of frag_t                                                   (* Blkpat_frag *)
    | Quote of frag_t                                                   (* Blkpat_frag *)
    | Mathtex_blk of string                                             (* Blkpat_lit *)
    | Mathml_blk of string                                              (* Blkpat_lit *)
    | Source of string                                                  (* Blkpat_lit *)
    | Tabular of string * tabular_t                                     (* Blkpat_raw_tab *)
    | Subpage of frag_t                                                 (* Blkpat_frag *)
    | Verbatim of string                                                (* Blkpat_lit *)
    | Picture of string * string                                        (* Blkpat_raw_raw *)
    | Pullquote of seq_t option * frag_t                                (* Blkpat_seqopt_frag *)
    | Custom of Custom.kind_t option * string * seq_t option * frag_t
    | Equation of seq_t option * block_t                                (* Blkpat_seqopt_blk *)
    | Printout of seq_t option * block_t                                (* Blkpat_seqopt_blk *)
    | Table of seq_t option * block_t                                   (* Blkpat_seqopt_blk *)
    | Figure of seq_t option * block_t                                  (* Blkpat_seqopt_blk *)
    | Part of seq_t                                                     (* Blkpat_seq *)
    | Appendix                                                          (* Blkpat_empty *)
    | Section of int * seq_t                                            (* Blkpat_int_seq *)
    | Bibliography                                                      (* Blkpat_empty *)
    | Notes                                                             (* Blkpat_empty *)
    | Toc                                                               (* Blkpat_empty *)
    | Title of int * seq_t                                              (* Blkpat_int_seq *)
    | Abstract of frag_t                                                (* Blkpat_frag *)
    | Rule                                                              (* Blkpat_empty *)
    | Bib of bib_t                                                      (* Blkpat_bib *)
    | Note of frag_t                                                    (* Blkpat_frag *)
    | Macrodef of string * string * seq_t
    | Boxoutdef of string * seq_t option * string option
    | Theoremdef of string * seq_t * string option
    | Extcomm_blk of Ident.t * block_pattern_t

and qanda_t =
    | New_questioner of seq_t option
    | New_answerer of seq_t option
    | Same_questioner
    | Same_answerer

and block_pattern_t =
    | Blkpat_empty
    | Blkpat_seq of seq_t
    | Blkpat_lit of string
    | Blkpat_frag of frag_t
    | Blkpat_raw of string
    | Blkpat_raw_raw of string * string


(********************************************************************************)
(*  {2 The main type {!t} itself}                                               *)
(********************************************************************************)

type t = frag_t

