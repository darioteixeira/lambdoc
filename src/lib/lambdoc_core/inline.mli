(********************************************************************************)
(*	Inline.mli
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
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
	| `Uref of Uri.t * 'a nelist option					(* Reference to an external URI *)
	| `Bref of Book.isbn_t * Book.rating_t option * 'a nelist option	(* Reference to a book via its ISBN *)
	| `Nref of Anchor.t nelist						(* Reference to an end note *)
	| `Cref of Anchor.t nelist						(* Citation of a bibliography entry *)
	| `Dref of Anchor.t							(* "Dumb" reference to an internal element *)
	| `Sref of Anchor.t							(* Smart reference to an internal element *)
	| `Mref of Anchor.t * 'a nelist						(* Manual reference to an internal element *)
	] with sexp

type raw_inline_t = raw_inline_t inline_t with sexp
type seq_t = raw_inline_t nelist with sexp

type (+'a, +'b) t = private [< ('a, 'b) t inline_t ] with sexp


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

val plain: Ustring.t -> ([> `Composition ], [> `Nonlink ]) t
val entity: Entity.t -> ([> `Composition ], [> `Nonlink ]) t
val linebreak: unit -> ([> `Composition ], [> `Nonlink ]) t
val math: Math.t -> ([> `Composition ], [> `Nonlink ]) t
val glyph: Alias.t -> string -> ([> `Composition ], [> `Nonlink ]) t
val bold: ('a, 'b) t nelist -> ('a, 'b) t
val emph: ('a, 'b) t nelist -> ('a, 'b) t
val code: ('a, 'b) t nelist -> ('a, 'b) t
val caps: ('a, 'b) t nelist -> ('a, 'b) t
val ins: ('a, 'b) t nelist -> ('a, 'b) t
val del: ('a, 'b) t nelist -> ('a, 'b) t
val sup: ('a, 'b) t nelist -> ('a, 'b) t
val sub: ('a, 'b) t nelist -> ('a, 'b) t
val mbox: ('a, 'b) t nelist -> ('a, 'b) t
val span: Classname.t option -> ('a, 'b) t nelist -> ('a, 'b) t
val uref: Uri.t -> ('a, [< `Nonlink ]) t nelist option -> ('a, [> `Link ]) t
val bref: Book.isbn_t -> Book.rating_t option -> ('a, [< `Nonlink ]) t nelist option -> ('a, [> `Link ]) t
val nref: Anchor.t nelist -> ([> `Manuscript ], [> `Link ]) t
val cref: Anchor.t nelist -> ([> `Manuscript ], [> `Link ]) t
val dref: Anchor.t -> ([> `Manuscript ], [> `Link ]) t
val sref: Anchor.t -> ([> `Manuscript ], [> `Link ]) t
val mref: Anchor.t -> (_, [< `Nonlink ]) t nelist -> ([> `Manuscript ], [> `Link ]) t

val get_seq: (_, _) t nelist -> seq_t
val get_inlines: (_, _) t list -> raw_inline_t list

