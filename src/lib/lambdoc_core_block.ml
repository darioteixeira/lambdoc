(********************************************************************************)
(*  Lambdoc_core_block.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module Attr = Lambdoc_core_attr
module Basic = Lambdoc_core_basic
module Custom = Lambdoc_core_custom
module Heading = Lambdoc_core_heading
module Inline = Lambdoc_core_inline
module Level = Lambdoc_core_level
module Math = Lambdoc_core_math
module Qanda = Lambdoc_core_qanda
module Source = Lambdoc_core_source
module Tabular = Lambdoc_core_tabular
module Wrapper = Lambdoc_core_wrapper

open Sexplib.Std
open Basic


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type block =
    | Paragraph of Inline.seq
    | Itemize of frag list
    | Enumerate of frag list
    | Description of (Inline.seq * frag) list
    | Qanda of (Qanda.t * frag) list
    | Verse of frag
    | Quote of frag
    | Mathblk of Math.t
    | Source of Source.t
    | Tabular of Tabular.t
    | Subpage of frag
    | Verbatim of string
    | Picture of href * string * int option
    | Pullquote of Inline.seq option * frag
    | Boxout of Custom.Boxout.t * Inline.seq option * frag
    | Theorem of Custom.Theorem.t * Inline.seq option * frag
    | Equation of Wrapper.t * t
    | Printout of Wrapper.t * t
    | Table of Wrapper.t * t
    | Figure of Wrapper.t * t
    | Heading of Heading.t
    | Title of Level.title * Inline.seq
    | Abstract of frag
    | Rule

and t =
    {
    blk: block;
    attr: Attr.t;
    }

and frag = t list with sexp


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let paragraph   ?(attr = Attr.default) seq = {blk = Paragraph seq; attr}
let itemize     ?(attr = Attr.default) frags = {blk = Itemize frags; attr}
let enumerate   ?(attr = Attr.default) frags = {blk = Enumerate frags; attr}
let description ?(attr = Attr.default) dfrags = {blk = Description dfrags; attr}
let qanda       ?(attr = Attr.default) qafrags = {blk = Qanda qafrags; attr}
let verse       ?(attr = Attr.default) frag = {blk = Verse frag; attr}
let quote       ?(attr = Attr.default) frag = {blk = Quote frag; attr}
let mathblk     ?(attr = Attr.default) data = {blk = Mathblk data; attr}
let source      ?(attr = Attr.default) data = {blk = Source data; attr}
let tabular     ?(attr = Attr.default) data = {blk = Tabular data; attr}
let subpage     ?(attr = Attr.default) frag = {blk = Subpage frag; attr}
let verbatim    ?(attr = Attr.default) txt = {blk = Verbatim txt; attr}
let picture     ?(attr = Attr.default) href alt width = {blk = Picture (href, alt, width); attr}
let pullquote   ?(attr = Attr.default) maybe_seq frag = {blk = Pullquote (maybe_seq, frag); attr}
let boxout      ?(attr = Attr.default) data maybe_seq frag = {blk = Boxout (data, maybe_seq, frag); attr}
let theorem     ?(attr = Attr.default) data maybe_seq frag = {blk = Theorem (data, maybe_seq, frag); attr}
let equation    ?(attr = Attr.default) wrapper blk = {blk = Equation (wrapper, blk); attr}
let printout    ?(attr = Attr.default) wrapper blk = {blk = Printout (wrapper, blk); attr}
let table       ?(attr = Attr.default) wrapper blk = {blk = Table (wrapper, blk); attr}
let figure      ?(attr = Attr.default) wrapper blk = {blk = Figure (wrapper, blk); attr}
let heading     ?(attr = Attr.default) data = {blk = Heading data; attr}
let title       ?(attr = Attr.default) level seq = {blk = Title (level, seq); attr}
let abstract    ?(attr = Attr.default) frag = {blk = Abstract frag; attr}
let rule        ?(attr = Attr.default) () = {blk = Rule; attr}

