(********************************************************************************)
(*	Inline.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

TYPE_CONV_PATH "Inline"

open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type 'a inline_t =
	[ `Plain of plain_t
	| `Entity of entity_t
	| `Linebreak
	| `Math of Math.t
	| `Bold of 'a list
	| `Emph of 'a list
	| `Code of 'a list
	| `Caps of 'a list
	| `Ins of 'a list
	| `Del of 'a list
	| `Sup of 'a list
	| `Sub of 'a list
	| `Mbox of 'a list
	| `Link of link_t * 'a list option
	| `See of ref_t * ref_t list
	| `Cite of ref_t * ref_t list
	| `Ref of ref_t
	| `Sref of ref_t
	| `Mref of ref_t * 'a list
	] with sexp

type raw_inline_t = raw_inline_t inline_t with sexp
type seq_t = raw_inline_t list with sexp

type (+'a, +'b) t = ('a, 'b) t inline_t with sexp


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

let plain txt = `Plain txt
let entity ent = `Entity ent
let linebreak () = `Linebreak
let math data = `Math data
let bold seq = `Bold seq
let emph seq = `Emph seq
let code seq = `Code seq
let caps seq = `Caps seq
let ins seq = `Ins seq
let del seq = `Del seq
let sup seq = `Sup seq
let sub seq = `Sub seq
let mbox seq = `Mbox seq
let link lnk maybe_seq = `Link (lnk, maybe_seq)
let see (hd, tl) = `See (hd, tl)
let cite (hd, tl) = `Cite (hd, tl)
let ref ref = `Ref ref
let sref ref = `Sref ref
let mref ref seq = `Mref (ref, seq)

let get_seq seq = seq
let get_seqs (hd, tl) = (hd, tl)

