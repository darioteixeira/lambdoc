(********************************************************************************)
(*	Ast.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of the document AST that all parsers are supposed to generate.
*)

open Lambdoc_core
open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Parsing data associated with command tags}				*)
(********************************************************************************)

type command_t =
	{
	comm_tag: Ident.t option;
	comm_label: Ref.t option;
	comm_order: string option;
	comm_extra: string option;
	comm_linenum: int;
	}


(********************************************************************************)
(**	{2 Data types for inline context}					*)
(********************************************************************************)

type entity_t =
	| Ent_name of string
	| Ent_deci of string
	| Ent_hexa of string

type seq_t = inline_t list
 and inline_t = command_t * raw_inline_t
 and raw_inline_t =
	| Plain of string
	| Entity of entity_t
	| Linebreak
	| Mathtex_inl of string
	| Mathml_inl of string
	| Bold of seq_t
	| Emph of seq_t
	| Code of seq_t
	| Caps of seq_t
	| Ins of seq_t
	| Del of seq_t
	| Sup of seq_t
	| Sub of seq_t
	| Mbox of seq_t
	| Link of string * seq_t option
	| See of string	list
	| Cite of string list
	| Ref of string
	| Sref of string
	| Mref of string * seq_t
	| Macroarg of string
	| Macrocall of string * seq_t list


(********************************************************************************)
(**	{2 Data types for document blocks}					*)
(********************************************************************************)

type tabular_cell_t = command_t * string option * seq_t option

type tabular_row_t = command_t * tabular_cell_t list

type tabular_group_t = command_t option * tabular_row_t list

type tabular_t =
	{
	thead: tabular_group_t option;
	tfoot: tabular_group_t option;
	tbodies: tabular_group_t list;
	}

type customdef_t =
	| Anonymous
	| Unnumbered of seq_t
	| Numbered of seq_t * string

type caption_t = command_t * seq_t

type bib_t =
	{
	author: command_t * seq_t;
	title: command_t * seq_t;
	resource: command_t * seq_t;
	}

type frag_t = block_t list
 and block_t = command_t * raw_block_t
 and raw_block_t =
	| Paragraph of seq_t
	| Itemize of (command_t * frag_t) list
	| Enumerate of (command_t * frag_t) list
	| Description of (command_t * seq_t * frag_t) list
	| Qanda of ((command_t * qanda_t * frag_t) * (command_t * qanda_t * frag_t)) list
	| Verse of frag_t
	| Quote of frag_t
	| Mathtex_blk of string
	| Mathml_blk of string
	| Source of string
	| Tabular of string * tabular_t
	| Verbatim of string
	| Image of string * string  (* (src, alt) *)
	| Subpage of frag_t
	| Decor of block_t
	| Pullquote of seq_t option * frag_t
	| Custom of string * seq_t option * frag_t
	| Equation of caption_t option * block_t
	| Printout of caption_t option * block_t
	| Table of caption_t option * block_t
	| Figure of caption_t option * block_t
	| Part of seq_t
	| Appendix
	| Section of Level.hierarchical_t * seq_t
	| Bibliography
	| Notes
	| Toc
	| Title of Level.title_t * seq_t
	| Abstract of frag_t
	| Rule
	| Bib of bib_t
	| Note of frag_t
	| Macrodef of string * string * seq_t
	| Boxoutdef of string * customdef_t
	| Theoremdef of string * customdef_t

and qanda_t =
	| Different of seq_t option
	| Repeated


(********************************************************************************)
(*	{2 The main type {!t} itself}						*)
(********************************************************************************)

type t = frag_t

