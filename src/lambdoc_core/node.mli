(********************************************************************************)
(*	Interface file for Node module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	The node is the basic element of text.  It can either be plain text,
	an HTML entity, or text modified by some sort of decoration.
*)

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

type (+'a, +'b) t = private [< 'c node_t ] as 'c (*with sexp*)

val plain: plain_t -> ([> `Composition ], [> `Nonlink ]) t
val entity: entity_t -> ([> `Composition ], [> `Nonlink ]) t
val math: Math.t -> ([> `Composition ], [> `Nonlink ]) t
val bold: ('a, 'b) t list -> ('a, 'b) t
val emph: ('a, 'b) t list -> ('a, 'b) t
val mono: ('a, 'b) t list -> ('a, 'b) t
val caps: ('a, 'b) t list -> ('a, 'b) t
val thru: ('a, 'b) t list -> ('a, 'b) t
val sup: ('a, 'b) t list -> ('a, 'b) t
val sub: ('a, 'b) t list -> ('a, 'b) t
val mbox: ('a, 'b) t list -> ('a, 'b) t
val link: link_t -> ('a, 'b) t list -> ('a, 'b) t
val see: ref_t -> ([> `Manuscript ], [> `Link ]) t
val cite: ref_t -> ([> `Manuscript ], [> `Link ]) t
val ref: ref_t -> ([> `Manuscript ], [> `Link ]) t
val sref: ref_t -> ([> `Manuscript ], [> `Link ]) t
val mref: ref_t -> ('a, [< `Nonlink ]) t list -> ([> `Manuscript ], [> `Link ]) t

