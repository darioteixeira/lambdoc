(********************************************************************************)
(*	Inline.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning inline elements.
*)

open Prelude
open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type 'a inline_t =
	[ `Plain of string
	| `Entity of Entity.t
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
	| `Link of Uri.t * 'a list option
	| `See of Ref.t plus_t
	| `Cite of Ref.t plus_t
	| `Ref of Ref.t
	| `Sref of Ref.t
	| `Mref of Ref.t * 'a list
	] with sexp

type raw_inline_t = raw_inline_t inline_t with sexp
type seq_t = raw_inline_t list with sexp

type (+'a, +'b) t = private [< ('a, 'b) t inline_t ] with sexp


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

val plain: string -> ([> `Composition ], [> `Nonlink ]) t
val entity: Entity.t -> ([> `Composition ], [> `Nonlink ]) t
val linebreak: unit -> ([> `Composition ], [> `Nonlink ]) t
val math: Math.t -> ([> `Composition ], [> `Nonlink ]) t
val bold: ('a, 'b) t list -> ('a, 'b) t
val emph: ('a, 'b) t list -> ('a, 'b) t
val code: ('a, 'b) t list -> ('a, 'b) t
val caps: ('a, 'b) t list -> ('a, 'b) t
val ins: ('a, 'b) t list -> ('a, 'b) t
val del: ('a, 'b) t list -> ('a, 'b) t
val sup: ('a, 'b) t list -> ('a, 'b) t
val sub: ('a, 'b) t list -> ('a, 'b) t
val mbox: ('a, 'b) t list -> ('a, 'b) t
val link: Uri.t -> ('a, [< `Nonlink ]) t list option -> ('a, [> `Link ]) t
val see: Ref.t plus_t -> ([> `Manuscript ], [> `Link ]) t
val cite: Ref.t plus_t -> ([> `Manuscript ], [> `Link ]) t
val ref: Ref.t -> ([> `Manuscript ], [> `Link ]) t
val sref: Ref.t -> ([> `Manuscript ], [> `Link ]) t
val mref: Ref.t -> (_, [< `Nonlink ]) t list -> ([> `Manuscript ], [> `Link ]) t

val get_seq: (_, _) t list -> seq_t
val get_seqs: (_, _) t list plus_t -> seq_t plus_t

