(********************************************************************************)
(*	Inline.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

TYPE_CONV_PATH "Inline"

open Prelude
open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type 'a inline_t =
	[ `Plain of Ustring.t
	| `Entity of Entity.t
	| `Linebreak
	| `Math of Math.t
	| `Glyph of Alias.t * string
	| `Bold of 'a nelist
	| `Emph of 'a nelist
	| `Code of 'a nelist
	| `Caps of 'a nelist
	| `Ins of 'a nelist
	| `Del of 'a nelist
	| `Sup of 'a nelist
	| `Sub of 'a nelist
	| `Mbox of 'a nelist
	| `Span of Classname.t option * 'a nelist
	| `Uri of Uri.t * 'a nelist option
	| `Book of Book.info_t * 'a nelist option
	| `Nref of Ref.t nelist
	| `Cref of Ref.t nelist
	| `Dref of Ref.t
	| `Sref of Ref.t
	| `Mref of Ref.t * 'a nelist
	] with sexp

type raw_inline_t = raw_inline_t inline_t with sexp
type seq_t = raw_inline_t nelist with sexp

type (+'a, +'b) t = ('a, 'b) t inline_t with sexp


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

let plain txt = `Plain txt
let entity ent = `Entity ent
let linebreak () = `Linebreak
let math data = `Math data
let glyph alias alt = `Glyph (alias, alt)
let bold seq = `Bold seq
let emph seq = `Emph seq
let code seq = `Code seq
let caps seq = `Caps seq
let ins seq = `Ins seq
let del seq = `Del seq
let sup seq = `Sup seq
let sub seq = `Sub seq
let mbox seq = `Mbox seq
let span classname seq = `Span (classname, seq)
let uri uri maybe_seq = `Uri (uri, maybe_seq)
let book info maybe_seq = `Book (info, maybe_seq)
let nref (hd, tl) = `Nref (hd, tl)
let cref (hd, tl) = `Cref (hd, tl)
let dref ref = `Dref ref
let sref ref = `Sref ref
let mref ref seq = `Mref (ref, seq)

let get_seq seq = seq
let get_inlines xs = xs

