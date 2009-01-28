(********************************************************************************)
(*	Implementation file for Node module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	The node is the basic element of text.  It can either be plain text,
	an HTML entity, or text modified by some sort of decoration.
*)

TYPE_CONV_PATH "Node"

open Basic

type 'a node_t =
	[ `Plain of plain_t
	| `Entity of entity_t
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
	] (*with sexp*)

type seq_t = 'a node_t as 'a list

type (+'a, +'b) t = 'c node_t as 'c (*with sexp*)

let plain txt = `Plain txt
let entity txt = `Entity txt
let math math = `Math math
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

