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
(**	{2 Type definitions}							*)
(********************************************************************************)

type wrapper_order_t = (Order.ordinal_t, [ Order.ordinal_t Order.auto_given_t | Order.user_given_t ]) Order.t with sexp, bin_io

type wrapper_t = Label.t * wrapper_order_t * Inline.seq_t with sexp, bin_io

type 'a block_t =
	[ `Paragraph of Inline.seq_t
	| `Itemize of Bullet.t * 'a list plus_t
	| `Enumerate of Numbering.t * 'a list plus_t
	| `Description of (Inline.seq_t * 'a list) plus_t
	| `Quote of Alignment.t * 'a list
	| `Callout of Alignment.t * string option * Inline.seq_t option * 'a list
	| `Math of Alignment.t * Math.t
	| `Code of Alignment.t * Code.t
	| `Tabular of Alignment.t * Tabular.tabular_t
	| `Verbatim of Alignment.t * raw_t
	| `Bitmap of Alignment.t * Image.t
	| `Subpage of Alignment.t * 'a list
	| `Equation of wrapper_t * 'a
	| `Printout of wrapper_t * 'a
	| `Table of wrapper_t * 'a
	| `Figure of wrapper_t * 'a
	| `Heading of Heading.t
	| `Title of title_level_t * Inline.seq_t
	| `Abstract of 'a list
	| `Rule
	] with sexp, bin_io

type raw_block_t = raw_block_t block_t with sexp, bin_io
type frag_t = raw_block_t list with sexp, bin_io

type (+'a, +'b, +'c, +'d) t = ('a, 'b, 'c, 'd) t block_t with sexp, bin_io


(********************************************************************************)
(**	{3 Functions and values}						*)
(********************************************************************************)

let paragraph seq = `Paragraph (Inline.get_seq seq)
let itemize bullet (head_frag, tail_frags) = `Itemize (bullet, (head_frag, tail_frags))
let enumerate numbering (head_frag, tail_frags) = `Enumerate (numbering, (head_frag, tail_frags))
let description (hd, tl) = let conv (seq, frag) = (Inline.get_seq seq, frag) in `Description (conv hd, List.map conv tl)
let quote alignment frag = `Quote (alignment, frag)
let callout alignment maybe_classname maybe_seq frag = `Callout (alignment, maybe_classname, (maybe Inline.get_seq maybe_seq), frag)
let math alignment mth = `Math (alignment, mth)
let code alignment x = `Code (alignment, x)
let tabular alignment tab = `Tabular (alignment, Tabular.get_tabular tab)
let verbatim alignment txt = `Verbatim (alignment, txt)
let bitmap alignment img = `Bitmap (alignment, img)
let subpage alignment frag = `Subpage (alignment, frag)
let equation wrapper equation_blk = `Equation (wrapper, equation_blk)
let printout wrapper printout_blk = `Printout (wrapper, printout_blk)
let table wrapper table_blk = `Table (wrapper, table_blk)
let figure wrapper figure_blk = `Figure (wrapper, figure_blk)
let heading head = `Heading head
let title level seq = `Title (level, Inline.get_seq seq)
let abstract frag = `Abstract frag
let rule () = `Rule
let get_frag frag = frag

