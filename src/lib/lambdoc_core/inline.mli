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
	| Link of Uri.t * seq_t option						(** Reference to an external URI *)
	| Booklink of Book.isbn_t * Book.rating_t option * seq_t option		(** Reference to a book via its ISBN *)
	| See of Pointer.t nelist						(** Reference to an end note *)
	| Cite of Pointer.t nelist						(** Citation of a bibliography entry *)
	| Ref of Pointer.t * seq_t option					(** Dumb or manual reference to an internal element *)
	| Sref of Pointer.t							(** Smart reference to an internal element *)

and t =
	{
	inline: inline_t; 
	attr: Attr.t;
	}

and seq_t = t nelist with sexp


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

val plain:	?attr:Attr.t -> string -> t
val entity:	?attr:Attr.t -> Entity.t -> t
val linebreak:	?attr:Attr.t -> unit -> t
val mathinl:	?attr:Attr.t -> Math.t -> t
val glyph:	?attr:Attr.t -> Alias.t -> string -> t
val bold:	?attr:Attr.t -> seq_t -> t
val emph:	?attr:Attr.t -> seq_t -> t
val code:	?attr:Attr.t -> seq_t -> t
val caps:	?attr:Attr.t -> seq_t -> t
val ins:	?attr:Attr.t -> seq_t -> t
val del:	?attr:Attr.t -> seq_t -> t
val sup:	?attr:Attr.t -> seq_t -> t
val sub:	?attr:Attr.t -> seq_t -> t
val mbox:	?attr:Attr.t -> seq_t -> t
val span:	?attr:Attr.t -> seq_t -> t
val link:	?attr:Attr.t -> Uri.t -> seq_t option -> t
val booklink:	?attr:Attr.t -> Book.isbn_t -> Book.rating_t option -> seq_t option -> t
val see:	?attr:Attr.t -> Pointer.t nelist -> t
val cite:	?attr:Attr.t -> Pointer.t nelist -> t
val ref:	?attr:Attr.t -> Pointer.t -> seq_t option -> t
val sref:	?attr:Attr.t -> Pointer.t -> t

