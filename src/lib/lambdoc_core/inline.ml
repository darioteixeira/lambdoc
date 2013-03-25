(********************************************************************************)
(*	Inline.ml
	Copyright (c) 2009-2013 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Sexplib.Std
open Prelude
open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type inline_t =
	| Plain of string
	| Entity of Entity.t
	| Linebreak
	| Mathinl of Math.t
	| Glyph of Alias.t * string
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
	| Link of Uri.t * seq_t option
	| Booklink of Book.isbn_t * Book.rating_t option * seq_t option
	| See of Pointer.t nelist
	| Cite of Pointer.t nelist
	| Dref of Pointer.t * seq_t option
	| Sref of Pointer.t * seq_t option
	| Mref of Pointer.t * seq_t

and t =
	{
	inline: inline_t;
	attr: Classname.t list;
	}

and seq_t = t nelist with sexp


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

let plain ?(attr = Attr.default) txt = {inline = Plain txt; attr}
let entity ?(attr = Attr.default) ent = {inline = Entity ent; attr}
let linebreak ?(attr = Attr.default) () = {inline = Linebreak; attr}
let mathinl ?(attr = Attr.default) data = {inline = Mathinl data; attr}
let glyph ?(attr = Attr.default) alias alt = {inline = Glyph (alias, alt); attr}
let bold ?(attr = Attr.default) seq = {inline = Bold seq; attr}
let emph ?(attr = Attr.default) seq = {inline = Emph seq; attr}
let code ?(attr = Attr.default) seq = {inline = Code seq; attr}
let caps ?(attr = Attr.default) seq = {inline = Caps seq; attr}
let ins ?(attr = Attr.default) seq = {inline = Ins seq; attr}
let del ?(attr = Attr.default) seq = {inline = Del seq; attr}
let sup ?(attr = Attr.default) seq = {inline = Sup seq; attr}
let sub ?(attr = Attr.default) seq = {inline = Sub seq; attr}
let mbox ?(attr = Attr.default) seq = {inline = Mbox seq; attr}
let span ?(attr = Attr.default) seq = {inline = Span seq; attr}
let link ?(attr = Attr.default) uri maybe_seq = {inline = Link (uri, maybe_seq); attr}
let booklink ?(attr = Attr.default) isbn maybe_rating maybe_seq = {inline = Booklink (isbn, maybe_rating, maybe_seq); attr}
let see ?(attr = Attr.default) (hd, tl) = {inline = See (hd, tl); attr}
let cite ?(attr = Attr.default) (hd, tl) = {inline = Cite (hd, tl); attr}
let dref ?(attr = Attr.default) pointer maybe_seq = {inline = Dref (pointer, maybe_seq); attr}
let sref ?(attr = Attr.default) pointer maybe_seq = {inline = Sref (pointer, maybe_seq); attr}
let mref ?(attr = Attr.default) pointer seq = {inline = Mref (pointer, seq); attr}

