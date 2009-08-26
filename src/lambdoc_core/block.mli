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
	[ `Paragraph of bool * Inline.seq_t
	| `Itemize of Bullet.t * 'a list plus_t
	| `Enumerate of Numbering.t * 'a list plus_t
	| `Description of (Inline.seq_t * 'a list) plus_t
	| `Qanda of ((Inline.seq_t option * 'a list) * (Inline.seq_t option * 'a list)) plus_t
	| `Verse of 'a list
	| `Quote of 'a list
	| `Math of Alignment.t * Math.t
	| `Program of Alignment.t * Program.t
	| `Tabular of Alignment.t * Tabular.tabular_t
	| `Verbatim of Alignment.t * raw_t
	| `Bitmap of Alignment.t * Image.t
	| `Subpage of Alignment.t * 'a list
	| `Pullquote of Alignment.t * 'a list
	| `Boxout of Alignment.t * string option * Inline.seq_t option * 'a list
	| `Equation of wrapper_t * 'a
	| `Printout of wrapper_t * 'a
	| `Table of wrapper_t * 'a
	| `Figure of wrapper_t * 'a
	| `Heading of Heading.heading_t
	| `Title of title_level_t * Inline.seq_t
	| `Abstract of 'a list
	| `Rule
	] with sexp

type raw_block_t = raw_block_t block_t with sexp
type frag_t = raw_block_t list with sexp

type (+'a, +'b, +'c, +'d, +'e) t = private [< ('a, 'b, 'c, 'd, 'e) t block_t ] with sexp


(********************************************************************************)
(**	{3 Public functions and values}						*)
(********************************************************************************)

(**	The functions in this module use phantom types to enforce some invariants
	on block nesting.  The main type {!t} has five of these phantom types:
	{ul	{li ['a] is either [`Composition] or [`Manuscript].  The former
		does not allow for elements that may produce internal links in
		the document, whereas the latter allows everything.}
		{li ['b] is either [`Listable] or [`Non_listable].  A block is
		listable if it is a wrapper (equation, printout, table, or figure),
		a pullquote, a boxout, or embeddable.}
		{li ['c] is either [`Embeddable] or [`Non_embeddable].  A block
		is termed embeddable if it can be a child of quotes and boxouts.
		All floaters except for pullquotes and boxouts are embeddable.
		Note that all prose blocks are also embeddable.}
		{li ['d] is either [`Prose] or [`Non_prose].  A block is defined
		as prose if it generates only text or lists of text, but it is not
		a verse block.  Therefore, the only prose blocks are paragraphs
		and the three types of lists.}
		{li ['e] indicates the actual block type.}}
*)

val paragraph: bool -> ('a, _) Inline.t list ->
	('a, [> `Listable ], [> `Embeddable ], [> `Prose ], [> `Paragraph_blk ]) t

val itemize: Bullet.t -> ('a, [< `Listable ], 'c, 'd, _) t list plus_t ->
	('a, [> `Listable ], 'c, 'd, [> `Itemize_blk ]) t

val enumerate: Numbering.t -> ('a, [< `Listable ], 'c, 'd, _) t list plus_t ->
	('a, [> `Listable ], 'c, 'd, [> `Enumerate_blk ]) t

val description: (('a, _) Inline.t list * ('a, [< `Listable ], 'c, 'd, _) t list) plus_t ->
	('a, [> `Listable ], 'c, 'd, [> `Description_blk ]) t

val qanda: ((('a, _) Inline.t list option * ('a, [< `Listable ], 'c, _, _) t list) * (('a, _) Inline.t list option * ('a, [< `Listable ], 'c, _, _) t list)) plus_t ->
	('a, [> `Listable ], 'c, [> `Non_prose ], [> `Description_blk ]) t

val verse: ('a, _, _, _, [< `Paragraph_block ]) t list ->
	('a, [> `Listable ], [> `Embeddable ], [> `Non_prose], [> `Verse_blk ]) t

val quote: ('a, [< `Listable ], [< `Embeddable ], _, _) t list ->
	('a, [> `Listable ], [> `Embeddable], [> `Non_prose ], [> `Quote_blk ]) t

val math: Alignment.t -> Math.t ->
	([> `Composition ], [> `Listable ], [> `Embeddable ], [> `Non_prose ], [> `Math_blk ]) t

val program: Alignment.t -> Program.t ->
	([> `Composition ], [> `Listable ], [> `Embeddable ], [> `Non_prose ], [> `Program_blk ]) t

val tabular: Alignment.t -> 'a Tabular.t ->
	('a, [> `Listable ], [> `Embeddable ], [> `Non_prose ], [> `Tabular_blk ]) t

val verbatim: Alignment.t -> raw_t ->
	([> `Composition ], [> `Listable ], [> `Embeddable], [> `Non_prose ], [> `Verbatim_blk ]) t

val bitmap: Alignment.t -> Image.t ->
	([> `Composition ], [> `Listable ], [> `Embeddable ], [> `Non_prose ], [> `Bitmap_blk ]) t

val subpage: Alignment.t -> ('a, _, _, _, _) t list ->
	('a, [> `Listable ], [> `Embeddable ], [> `Non_prose ], [> `Subpage_blk ]) t

val pullquote: Alignment.t -> ('a, [< `Listable ], [< `Embeddable ], [< `Prose ], _) t list ->
	([> `Manuscript ], [> `Listable ], [> `Non_embeddable], [> `Non_prose ], [> `Pullquote_blk ]) t

val boxout: Alignment.t -> string option -> ('a, _) Inline.t list option -> ('a, [< `Listable ], [< `Embeddable ], _, _) t list ->
	([> `Manuscript ], [> `Listable ], [> `Non_embeddable ], [> `Non_prose ], [> `Pullquote_blk ]) t

val equation: wrapper_t -> (_, _, _, _, [< `Math_blk ]) t ->
	([> `Manuscript ], [> `Listable ], [> `Non_embeddable ], [> `Non_prose ], [> `Equation_blk ]) t

val printout: wrapper_t -> (_, _, _, _, [< `Program_blk ]) t ->
	([> `Manuscript ], [> `Listable ], [> `Non_embeddable ], [> `Non_prose ], [> `Printout_blk ]) t

val table: wrapper_t -> (_, _, _, _, [< `Tabular_blk ]) t ->
	([> `Manuscript ], [> `Listable ], [> `Non_embeddable ], [> `Non_prose ], [> `Table_blk ]) t

val figure: wrapper_t -> (_, _, _, _, [< `Verbatim_blk | `Bitmap_blk | `Subpage_blk ]) t ->
	([> `Manuscript ], [> `Listable ], [> `Non_embeddable ], [> `Non_prose ], [> `Figure_blk ]) t

val heading: ('a, 'b, 'c, 'd, 'e) Heading.t ->
	('a, 'b, 'c, 'd, 'e) t

val title: title_level_t -> (_, _) Inline.t list ->
	([> `Manuscript ], [> `Non_listable ], [> `Non_embeddable ], [> `Non_prose ], [> `Title_blk ]) t

val abstract: (_, [< `Listable ], [< `Embeddable ], [< `Prose ], _) t list ->
	([> `Manuscript ], [> `Non_listable ], [> `Non_embeddable ], [> `Non_prose ], [> `Abstract_blk ]) t

val rule: unit ->
	([> `Manuscript ], [> `Non_listable ], [> `Non_embeddable ], [> `Non_prose ], [> `Rule_blk ]) t

val get_frag: (_, _, _, _, _) t list ->
	frag_t

