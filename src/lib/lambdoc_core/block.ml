(********************************************************************************)
(*	Block.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Sexplib.Std
open Prelude
open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type 'a block_t =
	[ `Paragraph of bool * bool option * Inline.seq_t
	| `Itemize of Bullet.t * 'a nelist nelist
	| `Enumerate of Numbering.t * 'a nelist nelist
	| `Description of (Inline.seq_t * 'a nelist) nelist
	| `Qanda of ((Inline.seq_t option option * 'a nelist) * (Inline.seq_t option option * 'a nelist)) nelist
	| `Verse of 'a nelist
	| `Quote of 'a nelist
	| `Math of Math.t
	| `Source of Source.t
	| `Tabular of Tabular.tabular_t
	| `Subpage of 'a nelist
	| `Verbatim of int * string
	| `Picture of bool * int option * Alias.t * string
	| `Bookpic of Book.isbn_t * Book.rating_t option * Book.cover_t
	| `Decor of Floatation.t * 'a
	| `Pullquote of Floatation.t * Inline.seq_t option * 'a nelist
	| `Boxout of Floatation.t * Custom.Boxout.t * Inline.seq_t option * 'a nelist
	| `Theorem of Custom.Theorem.t * Inline.seq_t option * 'a nelist
	| `Equation of Floatation.t * Wrapper.t * 'a
	| `Printout of Floatation.t * Wrapper.t * 'a
	| `Table of Floatation.t * Wrapper.t * 'a
	| `Figure of Floatation.t * Wrapper.t * 'a
	| `Heading of Heading.heading_t
	| `Title of Level.title_t * Inline.seq_t
	| `Abstract of 'a nelist
	| `Rule
	] with sexp

type raw_block_t = raw_block_t block_t with sexp
type frag_t = raw_block_t nelist with sexp

type (+'a, +'b, +'c, +'d, +'e) t = ('a, 'b, 'c, 'd, 'e) t block_t with sexp


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

let paragraph initial indent seq =
	`Paragraph (initial, indent, Inline.get_seq seq)

let itemize bullet frags =
	`Itemize (bullet, frags)

let enumerate numbering frags =
	`Enumerate (numbering, frags)

let description frags =
	let conv (seq, frag) = (Inline.get_seq seq, frag)
	in `Description (nemap conv frags)

let qanda frags =
	let conv ((q_seq, q_frag), (a_seq, a_frag)) = ((maybe (maybe Inline.get_seq) q_seq, q_frag), (maybe (maybe Inline.get_seq) a_seq, a_frag))
	in `Qanda (nemap conv frags)

let verse frag =
	`Verse frag

let quote frag =
	`Quote frag

let math data =
	`Math data

let source data =
	`Source data

let tabular data =
	`Tabular (Tabular.get_tabular data)

let subpage frag =
	`Subpage frag

let verbatim mult data =
	`Verbatim (mult, data)

let picture frame width alias alt =
	`Picture (frame, width, alias, alt)

let bookpic isbn maybe_rating cover =
	`Bookpic (isbn, maybe_rating, cover)

let decor floatation blk =
	`Decor (floatation, blk)

let pullquote floatation maybe_seq frag =
	`Pullquote (floatation, (maybe Inline.get_seq maybe_seq), frag)

let boxout floatation data maybe_seq frag =
	`Boxout (floatation, data, (maybe Inline.get_seq maybe_seq), frag)

let theorem data maybe_seq frag =
	`Theorem (data, (maybe Inline.get_seq maybe_seq), frag)

let equation floatation wrapper blk =
	`Equation (floatation, wrapper, blk)

let printout floatation wrapper blk =
	`Printout (floatation, wrapper, blk)

let table floatation wrapper blk =
	`Table (floatation, wrapper, blk)

let figure floatation wrapper blk =
	`Figure (floatation, wrapper, blk)

let heading data =
	`Heading (Heading.get_heading data)

let title level seq =
	`Title (level, Inline.get_seq seq)

let abstract frag =
	`Abstract frag

let rule () =
	`Rule

let get_frag frag =
	frag

let get_blocks xs =
	xs

