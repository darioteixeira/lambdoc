(********************************************************************************)
(*	Block.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

TYPE_CONV_PATH "Block"

open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type 'a block_t =
	[ `Paragraph of bool * Inline.seq_t
	| `Itemize of Bullet.t * 'a list plus_t
	| `Enumerate of Numbering.t * 'a list plus_t
	| `Description of (Inline.seq_t * 'a list) plus_t
	| `Qanda of ((Inline.seq_t option option * 'a list) * (Inline.seq_t option option * 'a list)) plus_t
	| `Verse of 'a list
	| `Quote of 'a list
	| `Math of Math.t
	| `Source of Source.t
	| `Tabular of Tabular.tabular_t
	| `Verbatim of int * raw_t
	| `Image of Image.t
	| `Subpage of 'a list
	| `Decor of Floatation.t * 'a
	| `Pullquote of Floatation.t * Inline.seq_t option * 'a list
	| `Boxout of Floatation.t * Custom.Boxout.t * Inline.seq_t option * 'a list
	| `Theorem of Custom.Theorem.t * Inline.seq_t option * 'a list
	| `Equation of Floatation.t * Wrapper.t * Inline.seq_t option * 'a
	| `Printout of Floatation.t * Wrapper.t * Inline.seq_t option * 'a
	| `Table of Floatation.t * Wrapper.t * Inline.seq_t option * 'a
	| `Figure of Floatation.t * Wrapper.t * Inline.seq_t option * 'a
	| `Heading of Heading.heading_t
	| `Title of title_level_t * Inline.seq_t
	| `Abstract of 'a list
	| `Rule
	] with sexp

type raw_block_t = raw_block_t block_t with sexp
type frag_t = raw_block_t list with sexp

type (+'a, +'b, +'c, +'d, +'e) t = ('a, 'b, 'c, 'd, 'e) t block_t with sexp


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

let paragraph initial seq =
	`Paragraph (initial, Inline.get_seq seq)

let itemize bullet (head_frag, tail_frags) =
	`Itemize (bullet, (head_frag, tail_frags))

let enumerate numbering (head_frag, tail_frags) =
	`Enumerate (numbering, (head_frag, tail_frags))

let description (hd, tl) =
	let conv (seq, frag) = (Inline.get_seq seq, frag)
	in `Description (fplus conv hd tl)

let qanda (hd, tl) =
	let conv ((q_seq, q_frag), (a_seq, a_frag)) = ((maybe (maybe Inline.get_seq) q_seq, q_frag), (maybe (maybe Inline.get_seq) a_seq, a_frag))
	in `Qanda (fplus conv hd tl)

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

let verbatim mult data =
	`Verbatim (mult, data)

let image data =
	`Image data

let subpage frag =
	`Subpage frag

let decor floatation blk =
	`Decor (floatation, blk)

let pullquote floatation maybe_seq frag =
	`Pullquote (floatation, (maybe Inline.get_seq maybe_seq), frag)

let boxout floatation data maybe_seq frag =
	`Boxout (floatation, data, (maybe Inline.get_seq maybe_seq), frag)

let theorem data maybe_seq frag =
	`Theorem (data, (maybe Inline.get_seq maybe_seq), frag)

let equation floatation wrapper maybe_seq blk =
	`Equation (floatation, wrapper, (maybe Inline.get_seq maybe_seq), blk)

let printout floatation wrapper maybe_seq blk =
	`Printout (floatation, wrapper, (maybe Inline.get_seq maybe_seq), blk)

let table floatation wrapper maybe_seq blk =
	`Table (floatation, wrapper, (maybe Inline.get_seq maybe_seq), blk)

let figure floatation wrapper maybe_seq blk =
	`Figure (floatation, wrapper, (maybe Inline.get_seq maybe_seq), blk)

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

