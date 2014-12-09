(********************************************************************************)
(*	Block.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Sexplib.Std
open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type block_t =
	| Paragraph of Inline.seq_t
	| Itemize of frag_t list
	| Enumerate of frag_t list
	| Description of (Inline.seq_t * frag_t) list
	| Qanda of (Qanda.t * frag_t) list
	| Verse of frag_t
	| Quote of frag_t
	| Mathblk of Math.t
	| Source of Source.t
	| Tabular of Tabular.t
	| Subpage of frag_t
	| Verbatim of string
	| Picture of Href.t * string * int option
	| Pullquote of Inline.seq_t option * frag_t
	| Boxout of Custom.Boxout.t * Inline.seq_t option * frag_t
	| Theorem of Custom.Theorem.t * Inline.seq_t option * frag_t
	| Equation of Wrapper.t * t
	| Printout of Wrapper.t * t
	| Table of Wrapper.t * t
	| Figure of Wrapper.t * t
	| Heading of Heading.t
	| Title of Level.title_t * Inline.seq_t
	| Abstract of frag_t
	| Rule

and t =
	{
	blk: block_t;
	attr: Attr.t;
	}

and frag_t = t list with sexp


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

let paragraph	?(attr = Attr.default) seq = {blk = Paragraph seq; attr}
let itemize	?(attr = Attr.default) frags = {blk = Itemize frags; attr}
let enumerate	?(attr = Attr.default) frags = {blk = Enumerate frags; attr}
let description	?(attr = Attr.default) dfrags = {blk = Description dfrags; attr}
let qanda	?(attr = Attr.default) qafrags = {blk = Qanda qafrags; attr}
let verse	?(attr = Attr.default) frag = {blk = Verse frag; attr}
let quote	?(attr = Attr.default) frag = {blk = Quote frag; attr}
let mathblk	?(attr = Attr.default) data = {blk = Mathblk data; attr}
let source	?(attr = Attr.default) data = {blk = Source data; attr}
let tabular	?(attr = Attr.default) data = {blk = Tabular data; attr}
let subpage	?(attr = Attr.default) frag = {blk = Subpage frag; attr}
let verbatim	?(attr = Attr.default) txt = {blk = Verbatim txt; attr}
let picture	?(attr = Attr.default) href alt width = {blk = Picture (href, alt, width); attr}
let pullquote	?(attr = Attr.default) maybe_seq frag = {blk = Pullquote (maybe_seq, frag); attr}
let boxout	?(attr = Attr.default) data maybe_seq frag = {blk = Boxout (data, maybe_seq, frag); attr}
let theorem	?(attr = Attr.default) data maybe_seq frag = {blk = Theorem (data, maybe_seq, frag); attr}
let equation	?(attr = Attr.default) wrapper blk = {blk = Equation (wrapper, blk); attr}
let printout	?(attr = Attr.default) wrapper blk = {blk = Printout (wrapper, blk); attr}
let table	?(attr = Attr.default) wrapper blk = {blk = Table (wrapper, blk); attr}
let figure	?(attr = Attr.default) wrapper blk = {blk = Figure (wrapper, blk); attr}
let heading	?(attr = Attr.default) data = {blk = Heading data; attr}
let title	?(attr = Attr.default) level seq = {blk = Title (level, seq); attr}
let abstract	?(attr = Attr.default) frag = {blk = Abstract frag; attr}
let rule	?(attr = Attr.default) () = {blk = Rule; attr}

