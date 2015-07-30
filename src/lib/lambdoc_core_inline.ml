(********************************************************************************)
(*  Lambdoc_core_inline.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module Basic = Lambdoc_core_basic
module Math = Lambdoc_core_math

open Sexplib.Std
open Basic


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type inline_t =
    | Plain of string
    | Entity of Entity.t
    | Linebreak
    | Mathinl of Math.t
    | Glyph of Href.t * string
    | Bold of seq_t
    | Emph of seq_t
    | Code of seq_t
    | Caps of seq_t
    | Ins of seq_t
    | Del of seq_t
    | Sup of seq_t
    | Sub of seq_t
    | Mbox of seq_t
    | Span of seq_t
    | Link of Href.t * seq_t option
    | See of Pointer.t list
    | Cite of Pointer.t list
    | Dref of Pointer.t * seq_t option
    | Sref of Pointer.t * seq_t option
    | Mref of Pointer.t * seq_t

and t =
    {
    inl: inline_t;
    attr: Attr.t;
    }

and seq_t = t list with sexp


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let plain     ?(attr = Attr.default) txt = {inl = Plain txt; attr}
let entity    ?(attr = Attr.default) ent = {inl = Entity ent; attr}
let linebreak ?(attr = Attr.default) () = {inl = Linebreak; attr}
let mathinl   ?(attr = Attr.default) data = {inl = Mathinl data; attr}
let glyph     ?(attr = Attr.default) href alt = {inl = Glyph (href, alt); attr}
let bold      ?(attr = Attr.default) seq = {inl = Bold seq; attr}
let emph      ?(attr = Attr.default) seq = {inl = Emph seq; attr}
let code      ?(attr = Attr.default) seq = {inl = Code seq; attr}
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

