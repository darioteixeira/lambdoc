(********************************************************************************)
(*	Lambdoc_core_block.mli
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning block elements.
*)

open Lambdoc_core
open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type block_t =
	| Paragraph of Inline.seq_t
	| Itemize of frag_t list
	| Enumerate of frag_t list
	| Description of (Inline.seq_t * frag_t) list
	| Qanda of (Qanda.t * frag_t) list
	| Verse of frag_t
	| Quote of frag_t
	| Mathblk of Math.t
	| Source of Source.t
	| Tabular of Tabular.t
	| Subpage of frag_t
	| Verbatim of string
	| Picture of Href.t * string * int option
	| Pullquote of Inline.seq_t option * frag_t
	| Boxout of Custom.Boxout.t * Inline.seq_t option * frag_t
	| Theorem of Custom.Theorem.t * Inline.seq_t option * frag_t
	| Equation of Wrapper.t * t
	| Printout of Wrapper.t * t
	| Table of Wrapper.t * t
	| Figure of Wrapper.t * t
	| Heading of Heading.t
	| Title of Level.title_t * Inline.seq_t
	| Abstract of frag_t
	| Rule

and t =
	{
	blk: block_t;
	attr: Attr.t;
	}

and frag_t = t list with sexp


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

val paragraph:	 ?attr:Attr.t -> Inline.seq_t -> t
val itemize:	 ?attr:Attr.t -> frag_t list -> t
val enumerate:	 ?attr:Attr.t -> frag_t list -> t
val description: ?attr:Attr.t -> (Inline.seq_t * frag_t) list -> t
val qanda:	 ?attr:Attr.t -> (Qanda.t * frag_t) list -> t
val verse:	 ?attr:Attr.t -> frag_t -> t
val quote:	 ?attr:Attr.t -> frag_t -> t
val mathblk:	 ?attr:Attr.t -> Math.t -> t
val source:	 ?attr:Attr.t -> Source.t -> t
val tabular:	 ?attr:Attr.t -> Tabular.t -> t
val subpage:	 ?attr:Attr.t -> frag_t -> t
val verbatim:	 ?attr:Attr.t -> string -> t
val picture:	 ?attr:Attr.t -> Href.t -> string -> int option -> t
val pullquote:	 ?attr:Attr.t -> Inline.seq_t option -> frag_t -> t
val boxout:	 ?attr:Attr.t -> Custom.Boxout.t -> Inline.seq_t option -> frag_t -> t
val theorem:	 ?attr:Attr.t -> Custom.Theorem.t -> Inline.seq_t option -> frag_t -> t
val equation:	 ?attr:Attr.t -> Wrapper.t -> t -> t
val printout:	 ?attr:Attr.t -> Wrapper.t -> t -> t
val table:	 ?attr:Attr.t -> Wrapper.t -> t -> t
val figure:	 ?attr:Attr.t -> Wrapper.t -> t -> t
val heading:	 ?attr:Attr.t -> Heading.t -> t
val title:	 ?attr:Attr.t -> Level.title_t -> Inline.seq_t -> t
val abstract:	 ?attr:Attr.t -> frag_t -> t
val rule:	 ?attr:Attr.t -> unit -> t

