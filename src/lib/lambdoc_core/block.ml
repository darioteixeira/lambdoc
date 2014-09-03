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
	| Picture of int option * Href.t * string
	| Extern of Href.t
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
	block: block_t;
	attr: Attr.t;
	}

and frag_t = t list with sexp


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

let paragraph ?(attr = Attr.default) seq = {block = Paragraph seq; attr}
let itemize ?(attr = Attr.default) frags = {block = Itemize frags; attr}
let enumerate ?(attr = Attr.default) frags = {block = Enumerate frags; attr}
let description ?(attr = Attr.default) dfrags = {block = Description dfrags; attr}
let qanda ?(attr = Attr.default) qafrags = {block = Qanda qafrags; attr}
let verse ?(attr = Attr.default) frag = {block = Verse frag; attr}
let quote ?(attr = Attr.default) frag = {block = Quote frag; attr}
let mathblk ?(attr = Attr.default) data = {block = Mathblk data; attr}
let source ?(attr = Attr.default) data = {block = Source data; attr}
let tabular ?(attr = Attr.default) data = {block = Tabular data; attr}
let subpage ?(attr = Attr.default) frag = {block = Subpage frag; attr}
let verbatim ?(attr = Attr.default) txt = {block = Verbatim txt; attr}
let picture ?(attr = Attr.default) width href alt = {block = Picture (width, href, alt); attr}
let extern ?(attr = Attr.default) href = {block = Extern href; attr}
let pullquote ?(attr = Attr.default) maybe_seq frag = {block = Pullquote (maybe_seq, frag); attr}
let boxout ?(attr = Attr.default) data maybe_seq frag = {block = Boxout (data, maybe_seq, frag); attr}
let theorem ?(attr = Attr.default) data maybe_seq frag = {block = Theorem (data, maybe_seq, frag); attr}
let equation ?(attr = Attr.default) wrapper blk = {block = Equation (wrapper, blk); attr}
let printout ?(attr = Attr.default) wrapper blk = {block = Printout (wrapper, blk); attr}
let table ?(attr = Attr.default) wrapper blk = {block = Table (wrapper, blk); attr}
let figure ?(attr = Attr.default) wrapper blk = {block = Figure (wrapper, blk); attr}
let heading ?(attr = Attr.default) data = {block = Heading data; attr}
let title ?(attr = Attr.default) level seq = {block = Title (level, seq); attr}
let abstract ?(attr = Attr.default) frag = {block = Abstract frag; attr}
let rule ?(attr = Attr.default) () = {block = Rule; attr}

