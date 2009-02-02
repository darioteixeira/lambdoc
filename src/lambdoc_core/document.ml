(********************************************************************************)
(*	Implementation file for Elem module.

	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

TYPE_CONV_PATH "Elem"

open Basic


(********************************************************************************)
(**	{2 Definitions concerning inline elements}				*)
(********************************************************************************)

(********************************************************************************)
(**	{3 Type definitions}							*)
(********************************************************************************)

type 'a raw_inline_t =
	[ `Plain of plain_t
	| `Entity of entity_t
	| `Mathinl of Math.t
	| `Bold of 'a list
	| `Emph of 'a list
	| `Mono of 'a list
	| `Caps of 'a list
	| `Thru of 'a list
	| `Sup of 'a list
	| `Sub of 'a list
	| `Mbox of 'a list
	| `Link of link_t * 'a list
	| `See of ref_t
	| `Cite of ref_t
	| `Ref of ref_t
	| `Sref of ref_t
	| `Mref of ref_t * 'a list
	] (*with sexp*)

type seq_t = 'a raw_inline_t as 'a list (*with sexp*)

type (+'a, +'b) inline_t = 'c raw_inline_t as 'c (*with sexp*)


(********************************************************************************)
(**	{3 Functions and values}						*)
(********************************************************************************)

let plain txt = `Plain txt
let entity txt = `Entity txt
let mathinl mth = `Mathinl mth
let bold seq = `Bold seq
let emph seq = `Emph seq
let mono seq = `Mono seq
let caps seq = `Caps seq
let thru seq = `Thru seq
let sup seq = `Sup seq
let sub seq = `Sub seq
let mbox seq = `Mbox seq
let link lnk seq = `Link (lnk, seq)
let see ref = `See ref
let cite ref = `Cite ref
let ref ref = `Ref ref
let sref ref = `Sref ref
let mref ref seq = `Mref (ref, seq)


(********************************************************************************)
(**	{2 Definitions concerning tabular environments}				*)
(********************************************************************************)

(********************************************************************************)
(**	{3 Exceptions}								*)
(********************************************************************************)

exception Invalid_column_specifier of char


(********************************************************************************)
(**	{3 Type definitions}							*)
(********************************************************************************)

type tab_alignment_t =
	| Center
	| Left
	| Right
	| Justify
	(*with sexp*)

type tab_weight_t =
	| Normal
	| Strong
	(*with sexp*)

type tab_column_t = tab_alignment_t * tab_weight_t (*with sexp*)

type tab_row_t = seq_t plus_t (*with sexp*)

type tab_group_t = tab_row_t plus_t (*with sexp*)

type tabular_t =
	{
	tcols: tab_column_t array;
	thead: tab_group_t option;
	tfoot: tab_group_t option;
	tbodies: tab_group_t plus_t;
	} (*with sexp*)


(********************************************************************************)
(**	{3 Functions and values}						*)
(********************************************************************************)

let column_of_specifier = function
	| 'c' -> (Center, Normal)
	| 'C' -> (Center, Strong)
	| 'l' -> (Left, Normal)
	| 'L' -> (Left, Strong)
	| 'r' -> (Right, Normal)
	| 'R' -> (Right, Strong)
	| 'j' -> (Justify, Normal)
	| 'J' -> (Justify, Strong)
	| x   -> raise (Invalid_column_specifier x)

let alignment_to_string = function
	| Center	-> "center"
	| Left		-> "left"
	| Right		-> "right"
	| Justify	-> "justify"

let make_row (hd, tl) = Obj.magic (hd, tl)

let make_tabular tcols ?thead ?tfoot tbodies =
	{
	tcols = tcols;
	thead = thead;
	tfoot = tfoot;
	tbodies = tbodies;
	}


(********************************************************************************)
(**	{2 Definitions concerning block elements}				*)
(********************************************************************************)

(********************************************************************************)
(**	{3 Type definitions}							*)
(********************************************************************************)

type part_order_t = (Order.ordinal_t, [ Order.ordinal_t Order.auto_given_t | Order.user_given_t | Order.none_given_t ]) Order.t (*with sexp*)
type section_order_t = (Order.hierarchical_t, [Order.hierarchical_t Order.auto_given_t | Order.user_given_t | Order.none_given_t ]) Order.t (*with sexp*)
type wrapper_order_t = (Order.ordinal_t, [ Order.ordinal_t Order.auto_given_t | Order.user_given_t ]) Order.t (*with sexp*)

type image_t = bool * bool * int option * alias_t * string (*with sexp*)

type wrapper_t = Label.t * wrapper_order_t * seq_t (*with sexp*)

type part_content_t =
	[ `Custom of seq_t
	| `Appendix
	] (*with sexp*)

type section_content_t =
	[ `Custom of seq_t
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

type 'a raw_block_t =
	[ `Paragraph of seq_t
	| `Itemize of Bullet.t * 'a list plus_t
	| `Enumerate of Numbering.t * 'a list plus_t
	| `Quote of Alignment.t * 'a list
	| `Mathblk of Alignment.t * Math.t
	| `Code of Alignment.t * bool * bool * Code.t
	| `Tabular of Alignment.t * tabular_t
	| `Bitmap of Alignment.t * image_t
	| `Verbatim of Alignment.t * raw_t
	| `Subpage of Alignment.t * 'a list
	| `Equation of wrapper_t * 'a
	| `Printout of wrapper_t * 'a
	| `Table of wrapper_t * 'a
	| `Figure of wrapper_t * 'a
	| `Heading of heading_block_t
	| `Title of title_level_t * seq_t
	| `Abstract of 'a list
	| `Rule
	] (*with sexp*)

type frag_t = 'a raw_block_t as 'a list (*with sexp*)

type (+'a, +'b, +'c, +'d) block_t = 'e raw_block_t as 'e (*with sexp*)


(********************************************************************************)
(**	{3 Functions and values}						*)
(********************************************************************************)

let paragraph seq = `Paragraph seq
let itemize bullet (head_frag, tail_frags) = `Itemize (bullet, (head_frag, tail_frags))
let enumerate numbering (head_frag, tail_frags) = `Enumerate (numbering, (head_frag, tail_frags))
let quote alignment frag = `Quote (alignment, frag)
let mathblk alignment mth = `Mathblk (alignment, mth)
let code alignment linenums zebra txt = `Code (alignment, linenums, zebra, txt)
let verbatim alignment txt = `Verbatim (alignment, txt)
let tabular alignment tab = `Tabular (alignment, tab)
let bitmap alignment image = `Bitmap (alignment, image)
let subpage alignment frag = `Subpage (alignment, frag)
let equation wrapper equation_blk = `Equation (wrapper, equation_blk)
let printout wrapper printout_blk = `Printout (wrapper, printout_blk)
let table wrapper table_blk = `Table (wrapper, table_blk)
let figure wrapper figure_blk = `Figure (wrapper, figure_blk)
let part label order seq = `Heading (`Part (label, order, `Custom seq))
let section label order location level seq = `Heading (`Section (label, order, location, level, `Custom seq))
let appendix label = `Heading (`Part (label, Order.none (), `Appendix))
let bibliography label = `Heading (`Section (label, Order.none (), `Mainbody, `Level1, `Bibliography))
let notes label = `Heading (`Section (label, Order.none (), `Mainbody, `Level1, `Notes))
let toc label = `Heading (`Section (label, Order.none (), `Mainbody, `Level1, `Toc))
let title level seq = `Title (level, seq)
let abstract frag = `Abstract frag
let rule () = `Rule


(********************************************************************************)
(**	{2 Definitions concerning bibliographic entries}			*)
(********************************************************************************)

(********************************************************************************)
(**	{3 Type definitions}							*)
(********************************************************************************)

type bib_order_t = (Order.ordinal_t, Order.ordinal_t Order.auto_given_t) Order.t (*with sexp*)

type bib_t =
	{
	label: Label.t;
	order: bib_order_t;
	author: seq_t;
	title: seq_t;
	resource: seq_t;
	} (*with sexp*)


(********************************************************************************)
(**	{2 Definitions concerning notes}					*)
(********************************************************************************)

(********************************************************************************)
(**	{3 Type definitions}							*)
(********************************************************************************)

type note_order_t = (Order.ordinal_t, Order.ordinal_t Order.auto_given_t) Order.t (*with sexp*)

type note_t =
	{
	label: Label.t;
	order: note_order_t;
	content: frag_t;
	} (*with sexp*)

