(********************************************************************************)
(*  Lambdoc_reader_ast.mli
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
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

type command =
    {
    comm_tag: ident option;             (** Tag associated with command (some markups have anonymous, tagless commands) *)
    comm_label: pointer option;         (** Label parameter attached to command *)
    comm_order: string option;          (** Order parameter attached to command *)
    comm_style: string option;          (** Style parameter attached to command *)
    comm_linenum: int;                  (** Source line number where command was declared *)
    comm_originator: Attr.originator;   (** Where the command comes from *)
    }


(********************************************************************************)
(** {2 Data types for inline context}                                           *)
(********************************************************************************)

type seq = inline list
 and inline = command * raw_inline
 and raw_inline =
    | Plain of string                           (* Inlpat_raw *)
    | Entity of string                          (* Inlpat_raw *)
    | Linebreak                                 (* Inlpat_empty *)
    | Mathtex_inl of string                     (* Inlpat_raw *)
    | Mathml_inl of string                      (* Inlpat_raw *)
    | Code of string                            (* Inlpat_raw *)
    | Glyph of string * string * string option  (* Inlpat_raw_raw *)
    | Bold of seq                               (* Inlpat_seq *)
    | Emph of seq                               (* Inlpat_seq *)
    | Mono of seq                               (* Inlpat_seq *)
    | Caps of seq                               (* Inlpat_seq *)
    | Ins of seq                                (* Inlpat_seq *)
    | Del of seq                                (* Inlpat_seq *)
    | Sup of seq                                (* Inlpat_seq *)
    | Sub of seq                                (* Inlpat_seq *)
    | Mbox of seq                               (* Inlpat_seq *)
    | Span of seq                               (* Inlpat_seq *)
    | Link of string * seq option               (* Inlpat_raw_seqopt *)
    | See of string list                        (* Inlpat_rawlist *)
    | Cite of string list                       (* Inlpat_rawlist *)
    | Dref of string * seq option               (* Inlpat_raw_seqopt *)
    | Sref of string * seq option               (* Inlpat_raw_seqopt *)
    | Mref of string * seq                      (* Inlpat_raw_seq *)
    | Macroarg of string                        (* Inlpat_raw *)
    | Macrocall of string * seq list            (* Inlpat_raw_seqlist *)
    | Extcomm_inl of ident * inline_pattern

and inline_pattern =
    | Inlpat_empty
    | Inlpat_seq of seq
    | Inlpat_raw of string
    | Inlpat_raw_raw of string * string
    | Inlpat_raw_seq of string * seq
    | Inlpat_raw_seqopt of string * seq option


(********************************************************************************)
(** {2 Data types for document blocks}                                          *)
(********************************************************************************)

type tabular_cell = command * seq option

type tabular_row = command * tabular_cell list

type tabular_group = command * tabular_row list

type tabular =
    {
    thead: tabular_group option;
    tfoot: tabular_group option;
    tbodies: tabular_group list;
    }

type longbib =
    {
    author: command * seq;
    title: command * seq;
    resource: command * seq;
    }

type frag = block list
 and block = command * raw_block
 and raw_block =
    | Paragraph of seq                                              (* Blkpat_seq *)
    | Itemize of (command * frag) list
    | Enumerate of (command * frag) list
    | Description of (command * seq * frag) list
    | Qanda of (command * qanda * frag) list
    | Verse of frag                                                 (* Blkpat_frag *)
    | Quote of frag                                                 (* Blkpat_frag *)
    | Mathtex_blk of string                                         (* Blkpat_lit *)
    | Mathml_blk of string                                          (* Blkpat_lit *)
    | Source of string                                              (* Blkpat_lit *)
    | Tabular of tabular                                            (* Blkpat_tab *)
    | Subpage of frag                                               (* Blkpat_frag *)
    | Verbatim of string                                            (* Blkpat_lit *)
    | Picture of string * string * string option                    (* Blkpat_raw_raw *)
    | Pullquote of seq option * frag                                (* Blkpat_seqopt_frag *)
    | Custom of string * seq option * frag
    | Equation of seq option * block                                (* Blkpat_seqopt_blk *)
    | Printout of seq option * block                                (* Blkpat_seqopt_blk *)
    | Table of seq option * block                                   (* Blkpat_seqopt_blk *)
    | Figure of seq option * block                                  (* Blkpat_seqopt_blk *)
    | Part of seq                                                   (* Blkpat_seq *)
    | Appendix                                                      (* Blkpat_empty *)
    | Section of int * seq                                          (* Blkpat_int_seq *)
    | Bibliography                                                  (* Blkpat_empty *)
    | Notes                                                         (* Blkpat_empty *)
    | Toc                                                           (* Blkpat_empty *)
    | Title of int * seq                                            (* Blkpat_int_seq *)
    | Abstract of frag                                              (* Blkpat_frag *)
    | Rule                                                          (* Blkpat_empty *)
    | Shortbib of seq                                               (* Blkpat_seq *)
    | Longbib of longbib                                            (* Blkpat_bib *)
    | Note of frag                                                  (* Blkpat_frag *)
    | Macrodef of string * string * seq
    | Boxoutdef of string * seq option * string option
    | Theoremdef of string * seq * string option
    | Extcomm_blk of ident * block_pattern

and qanda =
    | New_questioner of seq option
    | New_answerer of seq option
    | Same_questioner
    | Same_answerer

and block_pattern =
    | Blkpat_empty
    | Blkpat_seq of seq
    | Blkpat_lit of string
    | Blkpat_frag of frag
    | Blkpat_raw of string
    | Blkpat_raw_raw of string * string


(********************************************************************************)
(*  {2 The main type {!t} itself}                                               *)
(********************************************************************************)

type t = frag

