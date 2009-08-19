(********************************************************************************)
(*	Inline.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning inline elements.
*)

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
	| `Code of 'a list
	| `Caps of 'a list
	| `Thru of 'a list
	| `Sup of 'a list
	| `Sub of 'a list
	| `Mbox of 'a list
	| `Link of link_t * 'a list option
	| `See of ref_t
	| `Cite of ref_t
	| `Ref of ref_t
	| `Sref of ref_t
	| `Mref of ref_t * 'a list
	] with sexp

type raw_inline_t = raw_inline_t inline_t with sexp
type seq_t = raw_inline_t list with sexp

type (+'a, +'b) t = private [< ('a, 'b) t inline_t ] with sexp


(********************************************************************************)
(**	{3 Public functions and values}						*)
(********************************************************************************)

val plain: plain_t -> ([> `Composition ], [> `Nonlink ]) t
val entity: entity_t -> ([> `Composition ], [> `Nonlink ]) t
val linebreak: unit -> ([> `Composition ], [> `Nonlink ]) t
val math: Math.t -> ([> `Composition ], [> `Nonlink ]) t
val bold: ('a, 'b) t list -> ('a, 'b) t
val emph: ('a, 'b) t list -> ('a, 'b) t
val code: ('a, 'b) t list -> ('a, 'b) t
val caps: ('a, 'b) t list -> ('a, 'b) t
val thru: ('a, 'b) t list -> ('a, 'b) t
val sup: ('a, 'b) t list -> ('a, 'b) t
val sub: ('a, 'b) t list -> ('a, 'b) t
val mbox: ('a, 'b) t list -> ('a, 'b) t
val link: link_t -> ('a, [< `Nonlink ]) t list option -> ('a, [> `Link ]) t
val see: ref_t -> ([> `Manuscript ], [> `Link ]) t
val cite: ref_t -> ([> `Manuscript ], [> `Link ]) t
val ref: ref_t -> ([> `Manuscript ], [> `Link ]) t
val sref: ref_t -> ([> `Manuscript ], [> `Link ]) t
val mref: ref_t -> ('a, [< `Nonlink ]) t list -> ([> `Manuscript ], [> `Link ]) t

val get_seq: (_, _) t list -> seq_t
val get_seqs: (_, _) t list plus_t -> seq_t plus_t

