(********************************************************************************)
(*	Block.mli
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

(**	Definition of the ordering types for wrapper blocks.
*)
type wrapper_order_t = (Order.ordinal_t, [ Order.ordinal_t Order.auto_given_t | Order.user_given_t ]) Order.t with sexp


(**	The tuple of all common fields to wrappers.  The fields
	are the wrapper's label, its order, and a caption.
*)
type wrapper_t = Label.t * wrapper_order_t * Inline.seq_t with sexp


(**	The various types of individual building blocks.
*)
type 'a block_t =
	[ `Paragraph of Inline.seq_t
	| `Itemize of Bullet.t * 'a list plus_t
	| `Enumerate of Numbering.t * 'a list plus_t
	| `Description of (Inline.seq_t * 'a list) plus_t
	| `Quote of 'a list
	| `Pullquote of Alignment.t * 'a list
	| `Boxout of Alignment.t * string option * Inline.seq_t option * 'a list
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
	] with sexp

type raw_block_t = raw_block_t block_t with sexp
type frag_t = raw_block_t list with sexp

type (+'a, +'b, +'c, +'d) t = private [< ('a, 'b, 'c, 'd) t block_t ] with sexp


(********************************************************************************)
(**	{3 Public functions and values}						*)
(********************************************************************************)

(**	The functions in this module use phantom types to enforce some invariants
	on block nesting.  The main type {!t} has four of these phantom types:
	{ul	{li ['a] is either [`Composition] or [`Manuscript].  The former
		does not allow for elements that may produce internal links in
		the document, whereas the latter allows everything.}
		{li ['b] is either [`Embeddable] or [`Non_embeddable].  A block
		is termed embeddable if it can be a child of quotes, boxouts,
		lists, etc.}
		{li ['c] is either [`Nestable] or [`Non_nestable].  A block is
		nestable if it is embeddable or if it is a wrapper (equation,
		figure, etc).}
		{li ['d] indicates the actual block type.}}
*)

val paragraph: ('a, _) Inline.t list ->
	('a, [> `Embeddable ], [> `Nestable ], [> `Paragraph_blk ]) t

val itemize: Bullet.t -> ('a, 'b, [< `Nestable ], _) t list plus_t ->
	('a, 'b, [> `Nestable ], [> `Itemize_blk ]) t

val enumerate: Numbering.t -> ('a, 'b, [< `Nestable ], _) t list plus_t ->
	('a, 'b, [> `Nestable ], [> `Itemize_blk ]) t

val description: (('a, _) Inline.t list * ('a, 'b, [< `Nestable ], _) t list) plus_t ->
	('a, 'b, [> `Nestable ], [> `Description_blk ]) t

val quote: ('a, [< `Embeddable ], [< `Nestable ], _) t list ->
	('a, [> `Embeddable ], [> `Nestable], [> `Quote_blk ]) t

val pullquote: Alignment.t -> ('a, [< `Embeddable ], [< `Nestable ], _) t list ->
	('a, [> `Embeddable ], [> `Nestable], [> `Pullquote_blk ]) t

val boxout: Alignment.t -> string option -> ('a, _) Inline.t list option -> ('a, [< `Embeddable ], [< `Nestable ], _) t list ->
	('a, [> `Embeddable ], [> `Nestable], [> `Pullquote_blk ]) t

val math: Alignment.t -> Math.t ->
	([> `Composition ], [> `Embeddable ], [> `Nestable], [> `Math_blk ]) t

val code: Alignment.t -> Code.t ->
	([> `Composition ], [> `Embeddable ], [> `Nestable], [> `Code_blk ]) t

val tabular: Alignment.t -> 'a Tabular.t ->
	('a, [> `Embeddable ], [> `Nestable], [> `Tabular_blk ]) t

val verbatim: Alignment.t -> raw_t ->
	([> `Composition ], [> `Embeddable ], [> `Nestable], [> `Verbatim_blk ]) t

val bitmap: Alignment.t -> Image.t ->
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

val heading: Heading.t ->
	([> `Manuscript ], [> `Non_embeddable ], [> `Non_nestable], [> `Heading_blk ]) t

val title: title_level_t -> (_, _) Inline.t list ->
	([> `Manuscript ], [> `Non_embeddable ], [> `Non_nestable], [> `Title_blk ]) t

val abstract: (_, _, _, [< `Paragraph_blk ]) t list ->
	([> `Manuscript ], [> `Non_embeddable ], [> `Non_nestable], [> `Abstract_blk ]) t

val rule: unit ->
	([> `Manuscript ], [> `Non_embeddable ], [> `Non_nestable], [> `Rule_blk ]) t

val get_frag: (_, _, _, _) t list ->
	frag_t

