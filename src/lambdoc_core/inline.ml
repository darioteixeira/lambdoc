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
(**	{2 Type definitions}							*)
(********************************************************************************)

type 'a inline_t =
	[ `Plain of plain_t
	| `Entity of entity_t
	| `Linebreak
	| `Math of Math.t
	| `Bold of 'a list
	| `Emph of 'a list
	| `Mono of 'a list
	| `Caps of 'a list
	| `Thru of 'a list
	| `Sup of 'a list
	| `Sub of 'a list
	| `Mbox of 'a list
	| `Link of link_t * 'a list
	| `See of ref_t
	| `Cite of ref_t
	| `Ref of ref_t
	| `Sref of ref_t
	| `Mref of ref_t * 'a list
	] with sexp

type raw_inline_t = raw_inline_t inline_t with sexp
type seq_t = raw_inline_t list with sexp

type (+'a, +'b) t = ('a, 'b) t inline_t with sexp


(********************************************************************************)
(**	{2 Functions and values}						*)
(********************************************************************************)

let plain txt = `Plain txt
let entity ent = `Entity ent
let linebreak () = `Linebreak
let math mth = `Math mth
let bold seq = `Bold seq
let emph seq = `Emph seq
let mono seq = `Mono seq
let caps seq = `Caps seq
let thru seq = `Thru seq
let sup seq = `Sup seq
let sub seq = `Sub seq
let mbox seq = `Mbox seq
let link lnk seq = `Link (lnk, seq)
let see ref = `See ref
let cite ref = `Cite ref
let ref ref = `Ref ref
let sref ref = `Sref ref
let mref ref seq = `Mref (ref, seq)

let get_seq seq = seq
let get_seqs (hd, tl) = (hd, tl)

