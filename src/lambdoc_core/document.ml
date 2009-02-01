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


(********************************************************************************)
(**	{2 Definitions concerning document targets}				*)
(********************************************************************************)

module Target =
struct
	(************************************************************************)
	(**	{3 Type definitions}						*)
	(************************************************************************)

	type wrapper_kind_t =
		| Printout_wrapper
		| Equation_wrapper
		| Figure_wrapper
		| Table_wrapper
		(*with sexp*)

	type visible_target_t =
		| Section_target of section_location_t * section_order_t
		| Part_target of part_order_t
		| Wrapper_target of wrapper_kind_t * wrapper_order_t
		(*with sexp*)

	type t =
		| Visible_target of visible_target_t
		| Bib_target of bib_order_t
		| Note_target of note_order_t
		(*with sexp*)


	(********************************************************************************)
	(**	{3 Functions and values}						*)
	(********************************************************************************)

	let section location order = Visible_target (Section_target (location, order))

	let part order = Visible_target (Part_target order)

	let printout order = Visible_target (Wrapper_target (Printout_wrapper, order))

	let equation order = Visible_target (Wrapper_target (Equation_wrapper, order))

	let figure order = Visible_target (Wrapper_target (Figure_wrapper, order))

	let table order = Visible_target (Wrapper_target (Table_wrapper, order))

	let bib order = Bib_target order

	let note order = Note_target order
end


(********************************************************************************)
(**	{2 Definitions concerning valid documents}				*)
(********************************************************************************)

module Valid =
struct
	(************************************************************************)
	(**	{3 Type definitions}						*)
	(************************************************************************)

	type 'a valid_t = private
		{
		content: frag_t;
		bibs: bib_t list;
		notes: note_t list;
		toc: heading_block_t list;
		labelmap: Labelmap.t;
		} (*with sexp*)

	type valid_manuscript_t = [`Manuscript] valid_t (*with sexp*)
	type valid_composition_t = [`Composition] valid_t (*with sexp*)


	(************************************************************************)
	(**	{3 Public functions and values}					*)
	(************************************************************************)

	let make_manuscript content bibs notes toc labelmap =
		{
		content = content;
		bibs = bibs;
		notes = notes;
		toc = toc;
		labelmap = labelmap;
		}

	let make_composition content =
		{
		content = content;
		bibs = [];
		notes = [];
		toc = [];
		labelmap = Labelmap.create ();
		}


	(************************************************************************)
	(**	{3 Serialisation facilities}					*)
	(************************************************************************)

	(*
	let serialize_manuscript doc =
		Sexplib.Sexp.to_string_mach (sexp_of_t Variety.sexp_of_t doc)

	let serialize_composition =
		serialize_manuscript

	let deserialize_manuscript str =
		t_of_sexp Variety.t_of_sexp (Sexplib.Sexp.of_string str)

	let deserialize_composition =
		deserialize_manuscript
	*)
end


(********************************************************************************)
(**	{2 Definitions concerning invalid documents}				*)
(********************************************************************************)

module Invalid =
struct
	(************************************************************************)
	(**	{3 Type definitions}						*)
	(************************************************************************)

	type 'a invalid_t = private Error.t list (*with sexp*)

	type invalid_manuscript_t = [`Manuscript] invalid_t (*with sexp*)
	type invalid_composition_t = [`Composition] invalid_t (*with sexp*)


	(************************************************************************)
	(**	{2 Public functions and values}					*)
	(************************************************************************)

	let make_invalid_manuscript errors = errors
	let make_invalid_composition errors = errors
end


(********************************************************************************)
(**	{2 Definitions concerning ambivalent documents}				*)
(********************************************************************************)

module Ambivalent =
struct
	(************************************************************************)
	(**	{3 Type definitions}						*)
	(************************************************************************)

	type 'a t =
		[ `Valid of 'a Valid.t
		| `Invalid of 'a Invalid.t
		] (*with sexp*)

	type manuscript_t = [`Manuscript] t (*with sexp*)

	type composition_t = [`Composition] t (*with sexp*)


	(************************************************************************)
	(**	{3 Functions and values}					*)
	(************************************************************************)

	let make_valid_manuscript content bibs notes toc labels =
		`Valid (Valid.make_manuscript content bibs notes toc labels)

	let make_valid_composition content =
		`Valid (Valid.make_composition content)

	let make_invalid_manuscript errors =
		`Invalid (Invalid.make_manuscript errors)

	let make_invalid_composition errors =
		`Invalid (Invalid.make_composition errors)

	(************************************************************************)
	(**	{3 Serialisation facilities}					*)
	(************************************************************************)

	(*
	let serialize_manuscript doc =
		Sexplib.Sexp.to_string_mach (sexp_of_t Variety.sexp_of_t doc)

	let serialize_composition =
		serialize_manuscript

	let deserialize_manuscript str =
		t_of_sexp Variety.t_of_sexp (Sexplib.Sexp.of_string str)

	let deserialize_composition =
		deserialize_manuscript
	*)
end

