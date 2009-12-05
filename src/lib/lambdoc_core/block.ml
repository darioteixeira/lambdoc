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
	| `Qanda of ((Inline.seq_t option * 'a list) * (Inline.seq_t option * 'a list)) plus_t
	| `Verse of 'a list
	| `Quote of 'a list
	| `Math of Math.t
	| `Program of Program.t
	| `Tabular of Tabular.tabular_t
	| `Subpage of 'a list
	| `Verbatim of raw_t
	| `Image of Image.t
	| `Pullquote of Floatation.t * 'a list
	| `Boxout of Floatation.t * Custom.Boxout.t * Inline.seq_t option * 'a list
	| `Theorem of Floatation.t * Custom.Theorem.t * Inline.seq_t option * 'a list
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
	let conv ((q_seq, q_frag), (a_seq, a_frag)) = ((maybe Inline.get_seq q_seq, q_frag), (maybe Inline.get_seq a_seq, a_frag))
	in `Qanda (fplus conv hd tl)

let verse frag =
	`Verse frag

let quote frag =
	`Quote frag

let math data =
	`Math data

let program data =
	`Program data

let tabular data =
	`Tabular (Tabular.get_tabular data)

let subpage frag =
	`Subpage frag

let verbatim data =
	`Verbatim data

let image data =
	`Image data

let pullquote floatation frag =
	`Pullquote (floatation, frag)

let boxout floatation data maybe_seq frag =
	`Boxout (floatation, data, (maybe Inline.get_seq maybe_seq), frag)

let theorem floatation data maybe_seq frag =
	`Theorem (floatation, data, (maybe Inline.get_seq maybe_seq), frag)

let equation floatation wrapper maybe_seq equation_blk =
	`Equation (floatation, wrapper, (maybe Inline.get_seq maybe_seq), equation_blk)

let printout floatation wrapper maybe_seq printout_blk =
	`Printout (floatation, wrapper, (maybe Inline.get_seq maybe_seq), printout_blk)

let table floatation wrapper maybe_seq table_blk =
	`Table (floatation, wrapper, (maybe Inline.get_seq maybe_seq), table_blk)

let figure floatation wrapper maybe_seq figure_blk =
	`Figure (floatation, wrapper, (maybe Inline.get_seq maybe_seq), figure_blk)

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

