(********************************************************************************)
(*	Interface file for Document module.

	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

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

type (+'a, +'b) inline_t = private [< 'c raw_inline_t ] as 'c (*with sexp*)


(********************************************************************************)
(**	{3 Public functions and values}						*)
(********************************************************************************)

val plain: plain_t -> ([> `Composition ], [> `Nonlink ]) inline_t
val entity: entity_t -> ([> `Composition ], [> `Nonlink ]) inline_t
val mathinl: Math.t -> ([> `Composition ], [> `Nonlink ]) inline_t
val bold: ('a, 'b) inline_t list -> ('a, 'b) inline_t
val emph: ('a, 'b) inline_t list -> ('a, 'b) inline_t
val mono: ('a, 'b) inline_t list -> ('a, 'b) inline_t
val caps: ('a, 'b) inline_t list -> ('a, 'b) inline_t
val thru: ('a, 'b) inline_t list -> ('a, 'b) inline_t
val sup: ('a, 'b) inline_t list -> ('a, 'b) inline_t
val sub: ('a, 'b) inline_t list -> ('a, 'b) inline_t
val mbox: ('a, 'b) inline_t list -> ('a, 'b) inline_t
val link: link_t -> ('a, 'b) inline_t list -> ('a, 'b) inline_t
val see: ref_t -> ([> `Manuscript ], [> `Link ]) inline_t
val cite: ref_t -> ([> `Manuscript ], [> `Link ]) inline_t
val ref: ref_t -> ([> `Manuscript ], [> `Link ]) inline_t
val sref: ref_t -> ([> `Manuscript ], [> `Link ]) inline_t
val mref: ref_t -> ('a, [< `Nonlink ]) inline_t list -> ([> `Manuscript ], [> `Link ]) inline_t


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
(**	{3 Public functions and values}						*)
(********************************************************************************)

val column_of_specifier: char -> tab_column_t

val alignment_to_string: tab_alignment_t -> string

val make_row: (_, _) inline_t list plus_t -> tab_row_t

val make_tabular: tab_column_t array -> ?thead:tab_group_t -> ?tfoot:tab_group_t -> tab_group_t plus_t -> tabular_t


(********************************************************************************)
(**	{2 Definitions concerning block elements}				*)
(********************************************************************************)

(********************************************************************************)
(**	{3 Type definitions}							*)
(********************************************************************************)

(**	Definition of the ordering types for the various kinds of blocks.
*)
type part_order_t = (Order.ordinal_t, [ Order.ordinal_t Order.auto_given_t | Order.user_given_t | Order.none_given_t ]) Order.t (*with sexp*)
type section_order_t = (Order.hierarchical_t, [Order.hierarchical_t Order.auto_given_t | Order.user_given_t | Order.none_given_t ]) Order.t (*with sexp*)
type wrapper_order_t = (Order.ordinal_t, [ Order.ordinal_t Order.auto_given_t | Order.user_given_t ]) Order.t (*with sexp*)


(**	Common definitions for image types (bitmap and vectorial pictures).
*)
type image_t = bool * bool * int option * alias_t * string (*with sexp*)


(**	The tuple of all common fields to wrappers.  The fields
	are the wrapper's label, its order, and a caption.
*)
type wrapper_t = Label.t * wrapper_order_t * seq_t (*with sexp*)


(**	Part content.
*)
type part_content_t =
	[ `Custom of seq_t
	| `Appendix
	] (*with sexp*)


(**	Section content.
*)
type section_content_t =
	[ `Custom of seq_t
	| `Bibliography
	| `Notes
	| `Toc
	] (*with sexp*)


(**	Section locations.
*)
type section_location_t =
	[ `Mainbody
	| `Appendixed
	] (*with sexp*)


(**	Heading blocks.
*)
type heading_block_t =
	[ `Part of Label.t * part_order_t * part_content_t
	| `Section of Label.t * section_order_t * section_location_t * hierarchical_level_t * section_content_t
	] (*with sexp*)


(**	The various types of individual building blocks.
*)
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

type (+'a, +'b, +'c, +'d) block_t = private [< 'e raw_block_t ] as 'e (*with sexp*)


(********************************************************************************)
(**	{3 Public functions and values}						*)
(********************************************************************************)

val paragraph: ('a, _) inline_t list ->
	('a, [> `Embeddable ], [> `Nestable ], [> `Paragraph_blk ]) block_t

val itemize: Bullet.t -> ('a, 'b, [< `Nestable ], _) block_t list plus_t ->
	('a, 'b, [> `Nestable ], [> `Itemize_blk ]) block_t

val enumerate: Numbering.t -> ('a, 'b, [< `Nestable ], _) block_t list plus_t ->
	('a, 'b, [> `Nestable ], [> `Itemize_blk ]) block_t

val quote: Alignment.t -> ('a, [< `Embeddable ], [< `Nestable ], _) block_t list ->
	('a, [> `Embeddable ], [> `Nestable], [> `Quote_blk ]) block_t

val mathblk: Alignment.t -> Math.t ->
	([> `Composition ], [> `Embeddable ], [> `Nestable], [> `Math_blk ]) block_t

val code: Alignment.t -> bool -> bool -> Code.t ->
	([> `Composition ], [> `Embeddable ], [> `Nestable], [> `Code_blk ]) block_t

val verbatim: Alignment.t -> raw_t ->
	([> `Composition ], [> `Embeddable ], [> `Nestable], [> `Verbatim_blk ]) block_t

val tabular: Alignment.t -> tabular_t ->
	([> `Composition ], [> `Embeddable ], [> `Nestable], [> `Tabular_blk ]) block_t

val bitmap: Alignment.t -> image_t ->
	([> `Composition ], [> `Embeddable ], [> `Nestable], [> `Bitmap_blk ]) block_t

val subpage: Alignment.t -> ('a, _, _, _) block_t list ->
	('a, [> `Embeddable ], [> `Nestable], [> `Subpage_blk ]) block_t

val equation: wrapper_t -> (_, _, _, [< `Math_blk ]) block_t ->
	([> `Manuscript], [> `Non_embeddable ], [> `Nestable], [> `Equation_blk ]) block_t

val printout: wrapper_t -> (_, _, _, [< `Code_blk ]) block_t ->
	([> `Manuscript], [> `Non_embeddable ], [> `Nestable], [> `Printout_blk ]) block_t

val table: wrapper_t -> (_, _, _, [< `Tabular_blk ]) block_t ->
	([> `Manuscript], [> `Non_embeddable ], [> `Nestable], [> `Table_blk ]) block_t

val figure: wrapper_t -> (_, _, _, [< `Verbatim_blk | `Bitmap_blk | `Subpage_blk ]) block_t ->
	([> `Manuscript], [> `Non_embeddable ], [> `Nestable], [> `Figure_blk ]) block_t

val part: Label.t -> part_order_t -> (_, _) inline_t list ->
	([> `Manuscript ], [> `Non_embeddable ], [> `Non_nestable], [> `Heading_blk ]) block_t

val section: Label.t -> section_order_t -> section_location_t -> hierarchical_level_t -> (_, _) inline_t list ->
	([> `Manuscript ], [> `Non_embeddable ], [> `Non_nestable], [> `Heading_blk ]) block_t

val appendix: Label.t ->
	([> `Manuscript ], [> `Non_embeddable ], [> `Non_nestable], [> `Heading_blk ]) block_t

val bibliography: Label.t ->
	([> `Manuscript ], [> `Non_embeddable ], [> `Non_nestable], [> `Heading_blk ]) block_t

val notes: Label.t -> 
	([> `Manuscript ], [> `Non_embeddable ], [> `Non_nestable], [> `Heading_blk ]) block_t

val toc: Label.t -> 
	([> `Manuscript ], [> `Non_embeddable ], [> `Non_nestable], [> `Heading_blk ]) block_t

val title: title_level_t -> (_, _) inline_t list ->
	([> `Manuscript ], [> `Non_embeddable ], [> `Non_nestable], [> `Title_blk ]) block_t

val abstract: (_, _, _, [< `Paragraph_blk ]) block_t list ->
	([> `Manuscript ], [> `Non_embeddable ], [> `Non_nestable], [> `Abstract_blk ]) block_t

val rule: unit ->
	([> `Manuscript ], [> `Non_embeddable ], [> `Non_nestable], [> `Rule_blk ]) block_t


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

module Target:
sig
	(************************************************************************)
	(**	{3 Type definitions}						*)
	(************************************************************************)

	(**	The various types of wrappers.
	*)
	type wrapper_kind_t =
		| Printout_wrapper
		| Equation_wrapper
		| Figure_wrapper
		| Table_wrapper
		(*with sexp*)


	(**	The various variations of visible targets.
	*)
	type visible_target_t =
		| Section_target of section_location_t * section_order_t
		| Part_target of part_order_t
		| Wrapper_target of wrapper_kind_t * wrapper_order_t
		(*with sexp*)


	(**	At the highest level, a target can either be visible (if it can be
		referenced by [\ref], [\sref], or [\mref]), a bibliography block
		(referenced by [\cite]), or a note block (referenced by [\see]).
	*)
	type t =
		| Visible_target of visible_target_t
		| Bib_target of bib_order_t
		| Note_target of note_order_t
		(*with sexp*)


	(************************************************************************)
	(**	{3 Public functions and values}					*)
	(************************************************************************)

	val section: section_location_t -> section_order_t -> t
	val part: part_order_t -> t
	val printout: wrapper_order_t -> t
	val equation: wrapper_order_t ->
	val figure: wrapper_order_t ->
	val table: wrapper_order_t ->
	val bib: bib_order_t ->
	val note: note_order_t ->
end


(********************************************************************************)
(**	{2 Definitions concerning valid documents}				*)
(********************************************************************************)

module Valid:
sig
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

	type manuscript_t = [ `Manuscript ] valid_t (*with sexp*)
	type composition_t = [ `Composition ] valid_t (*with sexp*)


	(************************************************************************)
	(**	{3 Public functions and values}					*)
	(************************************************************************)

	val make_manuscript:
		([< `Composition | `Manuscript ], _, _, _) block_t list ->
		bib_t list ->
		note_t list ->
		heading_block_t list ->
		Labelmap.t ->
		manuscript_t

	val make_composition:
		([< `Composition ], _, _, _) block_t list ->
		composition_t


	(************************************************************************)
	(**	{3 Serialisation facilities}					*)
	(************************************************************************)

	(*
	val serialize_manuscript: manuscript_t -> string
	val serialize_composition: composition_t -> string
	val deserialize_manuscript: string -> manuscript_t
	val deserialize_composition: string -> composition_t
	*)
end


(********************************************************************************)
(**	{2 Definitions concerning invalid documents}				*)
(********************************************************************************)

module Invalid:
sig
	(************************************************************************)
	(**	{3 Type definitions}						*)
	(************************************************************************)

	type 'a invalid_t = private Error.t list (*with sexp*)

	type manuscript_t = [ `Manuscript ] invalid_t (*with sexp*)
	type composition_t = [ `Composition ] invalid_t (*with sexp*)


	(************************************************************************)
	(**	{3 Public functions and values}					*)
	(************************************************************************)

	val make_manuscript: Error.t list -> manuscript_t
	val make_composition: Error.t list -> composition_t
end


(********************************************************************************)
(**	{2 Definitions concerning ambivalent documents}				*)
(********************************************************************************)

module Ambivalent:
sig
	(************************************************************************)
	(**	{3 Type definitions}						*)
	(************************************************************************)
	type 'a t =
		[ `Valid of 'a Valid.t
		| `Invalid of 'a Invalid.t
		] (*with sexp*)


	(************************************************************************)
	(**	{3 Public functions and values}					*)
	(************************************************************************)

	type manuscript_t = [ `Manuscript ] t (*with sexp*)
	type composition_t = [ `Composition ] t (*with sexp*)
end

