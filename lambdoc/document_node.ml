(********************************************************************************)
(**	Definition of document nodes.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

TYPE_CONV_PATH "Document"

open Document_basic
open Document_math


(********************************************************************************)
(**	{2 Node module}								*)
(********************************************************************************)

(**	The node is the basic element of text.  It can either be plain text,
	an HTML entity, or text modified by some sort of decoration.
*)
module rec Node:
sig
	type textual_node_t =
		[ `Plain of plain_t
		| `Entity of entity_t
		] with sexp

	type textual_seq_t = textual_node_t list with sexp
	type super_seq_t = Node.super_node_t list with sexp
	type nonlink_seq_t = Node.nonlink_node_t list with sexp

	type nonlink_node_t =
		[ textual_node_t
		| `Math of Math.t
		| `Bold of super_seq_t
		| `Emph of super_seq_t
		| `Mono of super_seq_t
		| `Caps of super_seq_t
		| `Thru of super_seq_t
		| `Sup of super_seq_t
		| `Sub of super_seq_t
		| `Box of super_seq_t
		] with sexp

	type link_node_t =
		[ `Link of link_t * nonlink_seq_t
		| `See of ref_t
		| `Cite of ref_t
		| `Ref of ref_t
		| `Sref of ref_t
		| `Mref of ref_t * nonlink_seq_t
		] with sexp

	type super_node_t = [ nonlink_node_t | link_node_t ] with sexp

	type (+'a, 'b) t = 'a constraint 'a = [< super_node_t ] with sexp

	val plain: plain_t -> ([> textual_node_t ], [> `Composition ]) t
	val entity: entity_t -> ([> textual_node_t ], [> `Composition ]) t
	val math: Math.t -> ([> nonlink_node_t ], [> `Composition ]) t
	val bold: ([< super_node_t ], 'b) t list -> ([> nonlink_node_t ], 'b) t
	val emph: ([< super_node_t ], 'b) t list -> ([> nonlink_node_t ], 'b) t
	val mono: ([< super_node_t ], 'b) t list -> ([> nonlink_node_t ], 'b) t
	val caps: ([< super_node_t ], 'b) t list -> ([> nonlink_node_t ], 'b) t
	val thru: ([< super_node_t ], 'b) t list -> ([> nonlink_node_t ], 'b) t
	val sup: ([< super_node_t ], 'b) t list -> ([> nonlink_node_t ], 'b) t
	val sub: ([< super_node_t ], 'b) t list -> ([> nonlink_node_t ], 'b) t
	val box: ([< super_node_t ], 'b) t list -> ([> nonlink_node_t ], 'b) t

	val link: link_t -> ([< nonlink_node_t], 'b) t list -> ([> link_node_t ], 'b) t
	val see: ref_t -> ([> link_node_t ], [> `Manuscript ]) t
	val cite: ref_t -> ([> link_node_t ], [> `Manuscript ]) t
	val ref: ref_t -> ([> link_node_t ], [> `Manuscript ]) t
	val sref: ref_t -> ([> link_node_t ], [> `Manuscript ]) t
	val mref: ref_t -> ([< nonlink_node_t], 'b) t list -> ([> link_node_t ], [> `Manuscript ]) t
end =
struct
	type textual_node_t =
		[ `Plain of plain_t
		| `Entity of entity_t
		] with sexp

	type textual_seq_t = textual_node_t list with sexp
	type super_seq_t = Node.super_node_t list with sexp
	type nonlink_seq_t = Node.nonlink_node_t list with sexp

	type nonlink_node_t =
		[ textual_node_t
		| `Math of Math.t
		| `Bold of super_seq_t
		| `Emph of super_seq_t
		| `Mono of super_seq_t
		| `Caps of super_seq_t
		| `Thru of super_seq_t
		| `Sup of super_seq_t
		| `Sub of super_seq_t
		| `Box of super_seq_t
		] with sexp

	type link_node_t =
		[ `Link of link_t * nonlink_seq_t
		| `See of ref_t
		| `Cite of ref_t
		| `Ref of ref_t
		| `Sref of ref_t
		| `Mref of ref_t * nonlink_seq_t
		] with sexp

	type super_node_t = [ nonlink_node_t | link_node_t ] with sexp

	type (+'a, 'b) t = 'a constraint 'a = [< super_node_t ] with sexp

	let plain txt = `Plain txt
	let entity txt = `Entity txt
	let math m = `Math m
	let bold seq = `Bold (seq :> (super_node_t, _) t list)
	let emph seq = `Emph (seq :> (super_node_t, _) t list)
	let mono seq = `Mono (seq :> (super_node_t, _) t list)
	let caps seq = `Caps (seq :> (super_node_t, _) t list)
	let thru seq = `Thru (seq :> (super_node_t, _) t list)
	let sup seq = `Sup (seq :> (super_node_t, _) t list)
	let sub seq = `Sub (seq :> (super_node_t, _) t list)
	let box seq = `Box (seq :> (super_node_t, _) t list)

	let link lnk seq = `Link (lnk, (seq :> (nonlink_node_t, _) t list))
	let see ref = `See ref
	let cite ref = `Cite ref
	let ref ref = `Ref ref
	let sref ref = `Sref ref
	let mref ref seq = `Mref (ref, (seq :> (nonlink_node_t, _) t list))
end

