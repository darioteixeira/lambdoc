(********************************************************************************)
(*	Implementation file for Block module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of document blocks.  Blocks are the higher-level structures
	found in a document.  They include paragraphs, figures, tables, etc.
*)

TYPE_CONV_PATH "Document"

open Basic

type part_order_t = (Order.ordinal_t, [ Order.ordinal_t Order.auto_given_t | Order.user_given_t | Order.none_given_t ]) Order.t (*with sexp*)
type section_order_t = (Order.hierarchical_t, [Order.hierarchical_t Order.auto_given_t | Order.user_given_t | Order.none_given_t ]) Order.t (*with sexp*)
type wrapper_order_t = (Order.ordinal_t, [ Order.ordinal_t Order.auto_given_t | Order.user_given_t ]) Order.t (*with sexp*)
type bib_order_t = (Order.ordinal_t, Order.ordinal_t Order.auto_given_t) Order.t (*with sexp*)
type note_order_t = (Order.ordinal_t, Order.ordinal_t Order.auto_given_t) Order.t (*with sexp*)

type image_t = bool * bool * int option * alias_t * string (*with sexp*)

type wrapper_t = Label.t * wrapper_order_t * Node.seq_t (*with sexp*)

type part_content_t =
	[ `Custom of Node.seq_t
	| `Appendix
	] (*with sexp*)

type section_content_t =
	[ `Custom of Node.seq_t
	| `Bibliography
	| `Notes
	| `Toc
	] (*with sexp*)

type section_location_t =
	[ `Mainbody
	| `Appendixed
	] (*with sexp*)

type heading_block_t =
	[ `Part of Label.t * part_order_t * part_content_t
	| `Section of Label.t * section_order_t * section_location_t * hierarchical_level_t * section_content_t
	] (*with sexp*)

type 'a block_t =
	[ `Paragraph of Node.seq_t
	| `Itemize of Bullet.t * 'a list plus_t
	| `Enumerate of Numbering.t * 'a list plus_t
	| `Quote of Alignment.t * 'a list
	| `Math of Alignment.t * Math.t
	| `Code of Alignment.t * bool * bool * Code.t
	| `Tabular of Alignment.t * Tabular.t
	| `Bitmap of Alignment.t * image_t
	| `Verbatim of Alignment.t * raw_t
	| `Subpage of Alignment.t * 'a list
	| `Equation of wrapper_t * 'a
	| `Printout of wrapper_t * 'a
	| `Table of wrapper_t * 'a
	| `Figure of wrapper_t * 'a
	| `Heading of heading_block_t
	| `Title of title_level_t * Node.seq_t
	| `Abstract of 'a list
	| `Rule
	] (*with sexp*)

type frag_t = 'a block_t as 'a list (*with sexp*)

type (+'a, +'b, +'c, +'d) t = 'e block_t as 'e (*with sexp*)

let paragraph seq = `Paragraph (Obj.magic seq)
let itemize bullet (head_frag, tail_frags) = `Itemize (bullet, (head_frag, tail_frags))
let enumerate numbering (head_frag, tail_frags) = `Enumerate (numbering, (head_frag, tail_frags))
let quote alignment frag = `Quote (alignment, frag)
let math alignment mth = `Math (alignment, mth)
let code alignment linenums zebra txt = `Code (alignment, linenums, zebra, txt)
let verbatim alignment txt = `Verbatim (alignment, txt)
let tabular alignment tab = `Tabular (alignment, tab)
let bitmap alignment image = `Bitmap (alignment, image)
let subpage alignment frag = `Subpage (alignment, frag)
let equation wrapper equation_blk = `Equation (wrapper, equation_blk)
let printout wrapper printout_blk = `Printout (wrapper, printout_blk)
let table wrapper table_blk = `Table (wrapper, table_blk)
let figure wrapper figure_blk = `Figure (wrapper, figure_blk)
let part label order seq = `Heading (`Part (label, order, `Custom (Obj.magic seq)))
let section label order location level seq = `Heading (`Section (label, order, location, level, `Custom (Obj.magic seq)))
let appendix label = `Heading (`Part (label, Order.none (), `Appendix))
let bibliography label = `Heading (`Section (label, Order.none (), `Mainbody, `Level1, `Bibliography))
let notes label = `Heading (`Section (label, Order.none (), `Mainbody, `Level1, `Notes))
let toc label = `Heading (`Section (label, Order.none (), `Mainbody, `Level1, `Toc))
let title level seq = `Title (level, Obj.magic seq)
let abstract frag = `Abstract frag
let rule () = `Rule

