(********************************************************************************)
(*  Lambdoc_core_inline.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module Attr = Lambdoc_core_attr
module Basic = Lambdoc_core_basic
module Math = Lambdoc_core_math
module Hilite = Lambdoc_core_hilite

open Sexplib.Std
open Basic


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type inline =
    | Plain of string
    | Entity of entity
    | Linebreak
    | Math_inl of Math.t
    | Code of Hilite.t
    | Glyph of href * string * string option
    | Bold of seq
    | Emph of seq
    | Mono of seq
    | Caps of seq
    | Ins of seq
    | Del of seq
    | Sup of seq
    | Sub of seq
    | Mbox of seq
    | Span of seq
    | Link of href * seq option
    | See of pointer list
    | Cite of pointer list
    | Dref of pointer * seq option
    | Sref of pointer * seq option
    | Mref of pointer * seq

and t =
    {
    inl: inline;
    attr: Attr.t;
    }

and seq = t list [@@deriving sexp]


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let plain     ?(attr = Attr.default) txt = {inl = Plain txt; attr}
let entity    ?(attr = Attr.default) ent = {inl = Entity ent; attr}
let linebreak ?(attr = Attr.default) () = {inl = Linebreak; attr}
let math_inl  ?(attr = Attr.default) data = {inl = Math_inl data; attr}
let code      ?(attr = Attr.default) data = {inl = Code data; attr}
let glyph     ?(attr = Attr.default) href alt title = {inl = Glyph (href, alt, title); attr}
let bold      ?(attr = Attr.default) seq = {inl = Bold seq; attr}
let emph      ?(attr = Attr.default) seq = {inl = Emph seq; attr}
let mono      ?(attr = Attr.default) seq = {inl = Mono seq; attr}
let caps      ?(attr = Attr.default) seq = {inl = Caps seq; attr}
let ins       ?(attr = Attr.default) seq = {inl = Ins seq; attr}
let del       ?(attr = Attr.default) seq = {inl = Del seq; attr}
let sup       ?(attr = Attr.default) seq = {inl = Sup seq; attr}
let sub       ?(attr = Attr.default) seq = {inl = Sub seq; attr}
let mbox      ?(attr = Attr.default) seq = {inl = Mbox seq; attr}
let span      ?(attr = Attr.default) seq = {inl = Span seq; attr}
let link      ?(attr = Attr.default) href maybe_seq = {inl = Link (href, maybe_seq); attr}
let see       ?(attr = Attr.default) pointers = {inl = See pointers; attr}
let cite      ?(attr = Attr.default) pointers = {inl = Cite pointers; attr}
let dref      ?(attr = Attr.default) pointer maybe_seq = {inl = Dref (pointer, maybe_seq); attr}
let sref      ?(attr = Attr.default) pointer maybe_seq = {inl = Sref (pointer, maybe_seq); attr}
let mref      ?(attr = Attr.default) pointer seq = {inl = Mref (pointer, seq); attr}

