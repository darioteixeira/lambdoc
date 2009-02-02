(********************************************************************************)
(*	Interface file for Block module.

	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning block elements.
*)

open Basic


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

(**	Definition of the ordering types for the various kinds of blocks.
*)
type part_order_t = (Order.ordinal_t, [ Order.ordinal_t Order.auto_given_t | Order.user_given_t | Order.none_given_t ]) Order.t (*with sexp*)
type section_order_t = (Order.hierarchical_t, [Order.hierarchical_t Order.auto_given_t | Order.user_given_t | Order.none_given_t ]) Order.t (*with sexp*)
type wrapper_order_t = (Order.ordinal_t, [ Order.ordinal_t Order.auto_given_t | Order.user_given_t ]) Order.t (*with sexp*)
type bib_order_t = (Order.ordinal_t, Order.ordinal_t Order.auto_given_t) Order.t (*with sexp*)
type note_order_t = (Order.ordinal_t, Order.ordinal_t Order.auto_given_t) Order.t (*with sexp*)


(**	Common definitions for image types (bitmap and vectorial pictures).
*)
type image_t = bool * bool * int option * alias_t * string (*with sexp*)


(**	The tuple of all common fields to wrappers.  The fields
	are the wrapper's label, its order, and a caption.
*)
type wrapper_t = Label.t * wrapper_order_t * Inline.seq_t (*with sexp*)


(**	Part content.
*)
type part_content_t =
	[ `Custom of Inline.seq_t
	| `Appendix
	] (*with sexp*)


(**	Section content.
*)
type section_content_t =
	[ `Custom of Inline.seq_t
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
type 'a block_t =
	[ `Paragraph of Inline.seq_t
	| `Itemize of Bullet.t * 'a list plus_t
	| `Enumerate of Numbering.t * 'a list plus_t
	| `Quote of Alignment.t * 'a list
	| `Mathblk of Alignment.t * Math.t
	| `Code of Alignment.t * bool * bool * Code.t
	| `Tabular of Alignment.t * Tabular.tabular_t
	| `Bitmap of Alignment.t * image_t
	| `Verbatim of Alignment.t * raw_t
	| `Subpage of Alignment.t * 'a list
	| `Equation of wrapper_t * 'a
	| `Printout of wrapper_t * 'a
	| `Table of wrapper_t * 'a
	| `Figure of wrapper_t * 'a
	| `Heading of heading_block_t
	| `Title of title_level_t * Inline.seq_t
	| `Abstract of 'a list
	| `Rule
	] (*with sexp*)


type frag_t = ('a block_t as 'a) list (*with sexp*)


type (+'a, +'b, +'c, +'d) t = private [< 'e block_t ] as 'e (*with sexp*)


type bib_t =
	{
	label: Label.t;
	order: bib_order_t;
	author: Inline.seq_t;
	title: Inline.seq_t;
	resource: Inline.seq_t;
	} (*with sexp*)


type note_t =
	{
	label: Label.t;
	order: note_order_t;
	content: frag_t;
	} (*with sexp*)


(********************************************************************************)
(**	{3 Public functions and values}						*)
(********************************************************************************)

val paragraph: ('a, _) Inline.t list ->
	('a, [> `Embeddable ], [> `Nestable ], [> `Paragraph_blk ]) t

val itemize: Bullet.t -> ('a, 'b, [< `Nestable ], _) t list plus_t ->
	('a, 'b, [> `Nestable ], [> `Itemize_blk ]) t

val enumerate: Numbering.t -> ('a, 'b, [< `Nestable ], _) t list plus_t ->
	('a, 'b, [> `Nestable ], [> `Itemize_blk ]) t

val quote: Alignment.t -> ('a, [< `Embeddable ], [< `Nestable ], _) t list ->
	('a, [> `Embeddable ], [> `Nestable], [> `Quote_blk ]) t

val mathblk: Alignment.t -> Math.t ->
	([> `Composition ], [> `Embeddable ], [> `Nestable], [> `Math_blk ]) t

val code: Alignment.t -> bool -> bool -> Code.t ->
	([> `Composition ], [> `Embeddable ], [> `Nestable], [> `Code_blk ]) t

val verbatim: Alignment.t -> raw_t ->
	([> `Composition ], [> `Embeddable ], [> `Nestable], [> `Verbatim_blk ]) t

val tabular: Alignment.t -> 'a Tabular.t ->
	('a, [> `Embeddable ], [> `Nestable], [> `Tabular_blk ]) t

val bitmap: Alignment.t -> image_t ->
	([> `Composition ], [> `Embeddable ], [> `Nestable], [> `Bitmap_blk ]) t

val subpage: Alignment.t -> ('a, _, _, _) t list ->
	('a, [> `Embeddable ], [> `Nestable], [> `Subpage_blk ]) t

val equation: wrapper_t -> (_, _, _, [< `Math_blk ]) t ->
	([> `Manuscript], [> `Non_embeddable ], [> `Nestable], [> `Equation_blk ]) t

val printout: wrapper_t -> (_, _, _, [< `Code_blk ]) t ->
	([> `Manuscript], [> `Non_embeddable ], [> `Nestable], [> `Printout_blk ]) t

val table: wrapper_t -> (_, _, _, [< `Tabular_blk ]) t ->
	([> `Manuscript], [> `Non_embeddable ], [> `Nestable], [> `Table_blk ]) t

val figure: wrapper_t -> (_, _, _, [< `Verbatim_blk | `Bitmap_blk | `Subpage_blk ]) t ->
	([> `Manuscript], [> `Non_embeddable ], [> `Nestable], [> `Figure_blk ]) t

val part: Label.t -> part_order_t -> (_, _) Inline.t list ->
	([> `Manuscript ], [> `Non_embeddable ], [> `Non_nestable], [> `Heading_blk ]) t

val section: Label.t -> section_order_t -> section_location_t -> hierarchical_level_t -> (_, _) Inline.t list ->
	([> `Manuscript ], [> `Non_embeddable ], [> `Non_nestable], [> `Heading_blk ]) t

val appendix: Label.t ->
	([> `Manuscript ], [> `Non_embeddable ], [> `Non_nestable], [> `Heading_blk ]) t

val bibliography: Label.t ->
	([> `Manuscript ], [> `Non_embeddable ], [> `Non_nestable], [> `Heading_blk ]) t

val notes: Label.t -> 
	([> `Manuscript ], [> `Non_embeddable ], [> `Non_nestable], [> `Heading_blk ]) t

val toc: Label.t -> 
	([> `Manuscript ], [> `Non_embeddable ], [> `Non_nestable], [> `Heading_blk ]) t

val title: title_level_t -> (_, _) Inline.t list ->
	([> `Manuscript ], [> `Non_embeddable ], [> `Non_nestable], [> `Title_blk ]) t

val abstract: (_, _, _, [< `Paragraph_blk ]) t list ->
	([> `Manuscript ], [> `Non_embeddable ], [> `Non_nestable], [> `Abstract_blk ]) t

val rule: unit ->
	([> `Manuscript ], [> `Non_embeddable ], [> `Non_nestable], [> `Rule_blk ]) t

val get_frag: (_, _, _, _) t list ->
	frag_t

