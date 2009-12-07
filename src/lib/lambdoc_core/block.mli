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
(**	{1 Type definitions}							*)
(********************************************************************************)

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
	| `Math of Math.t
	| `Source of Source.t
	| `Tabular of Tabular.tabular_t
	| `Verbatim of raw_t
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

type (+'a, +'b, +'c, +'d, +'e) t = private [< ('a, 'b, 'c, 'd, 'e) t block_t ] with sexp


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

(**	The functions in this module use phantom types to enforce some invariants
	on block nesting.  The main type {!t} has five of these phantom types:
	{ul	{li ['a] is either [`Composition] or [`Manuscript].  The former
		does not allow for elements that may produce internal links in
		the document, whereas the latter allows everything.}
		{li ['b] is either [`Listable] or [`Non_listable].  A block is
		listable if it is a wrapper (equation, printout, table, or figure),
		a pullquote, a boxout, a custom environment, or embeddable.}
		{li ['c] is either [`Embeddable] or [`Non_embeddable].  A block
		is termed embeddable if it can be a child of boxes.  Note that
		all textual blocks are also embeddable.}
		{li ['d] is either [`Textual] or [`Non_textual].  A block is
		defined as textual if it generates only text, mathematics, or
		lists of text.  Therefore, the only textual blocks are paragraphs
		and the three types of lists.}
		{li ['e] indicates the actual block type.}}
*)

val paragraph: bool -> ('a, _) Inline.t list ->
	('a, [> `Listable ], [> `Embeddable ], [> `Textual ], [> `Paragraph_blk ]) t

val itemize: Bullet.t -> ('a, [< `Listable ], 'c, 'd, _) t list plus_t ->
	('a, [> `Listable ], 'c, 'd, [> `Itemize_blk ]) t

val enumerate: Numbering.t -> ('a, [< `Listable ], 'c, 'd, _) t list plus_t ->
	('a, [> `Listable ], 'c, 'd, [> `Enumerate_blk ]) t

val description: (('a, _) Inline.t list * ('a, [< `Listable ], 'c, 'd, _) t list) plus_t ->
	('a, [> `Listable ], 'c, 'd, [> `Description_blk ]) t

val qanda: ((('a, _) Inline.t list option * ('a, [< `Listable ], 'c, _, _) t list) * (('a, _) Inline.t list option * ('a, [< `Listable ], 'c, _, _) t list)) plus_t ->
	('a, [> `Listable ], 'c, [> `Non_textual ], [> `Qanda_blk ]) t

val verse: ('a, _, _, _, [< `Paragraph_block ]) t list ->
	('a, [> `Listable ], [> `Embeddable ], [> `Textual], [> `Verse_blk ]) t

val quote: ('a, [< `Listable ], [< `Embeddable ], _, _) t list ->
	('a, [> `Listable ], [> `Embeddable], [> `Non_textual ], [> `Quote_blk ]) t

val math: Math.t ->
	([> `Composition ], [> `Listable ], [> `Embeddable ], [> `Textual ], [> `Math_blk ]) t

val source: Source.t ->
	([> `Composition ], [> `Listable ], [> `Embeddable ], [> `Non_textual ], [> `Source_blk ]) t

val tabular: 'a Tabular.t ->
	('a, [> `Listable ], [> `Embeddable ], [> `Non_textual ], [> `Tabular_blk ]) t

val verbatim: raw_t ->
	([> `Composition ], [> `Listable ], [> `Embeddable], [> `Non_textual ], [> `Verbatim_blk ]) t

val image: Image.t ->
	([> `Composition ], [> `Listable ], [> `Embeddable ], [> `Non_textual ], [> `Image_blk ]) t

val subpage: ('a, _, _, _, _) t list ->
	('a, [> `Listable ], [> `Embeddable ], [> `Non_textual ], [> `Subpage_blk ]) t

val decor: Floatation.t -> (_, _, _, _, [< `Verbatim_blk | `Image_blk ]) t ->
	([> `Manuscript ], [> `Listable ], [> `Embeddable], [> `Non_textual ], [> `Decor_blk ]) t

val pullquote: Floatation.t -> (_, _) Inline.t list option -> (_, [< `Listable ], [< `Embeddable ], [< `Textual ], _) t list ->
	([> `Manuscript ], [> `Listable ], [> `Non_embeddable], [> `Non_textual ], [> `Pullquote_blk ]) t

val boxout: Floatation.t -> Custom.Boxout.t -> (_, _) Inline.t list option -> (_, [< `Listable ], [< `Embeddable ], _, _) t list ->
	([> `Manuscript ], [> `Listable ], [> `Non_embeddable ], [> `Non_textual ], [> `Boxout_blk ]) t

val theorem: Custom.Theorem.t -> (_, _) Inline.t list option -> (_, [< `Listable ], [< `Embeddable ], [< `Textual ], _) t list ->
	([> `Manuscript ], [> `Listable ], [> `Non_embeddable ], [> `Non_textual ], [> `Theorem_blk ]) t

val equation: Floatation.t -> Wrapper.t -> (_, _) Inline.t list option -> (_, _, _, _, [< `Math_blk ]) t ->
	([> `Manuscript ], [> `Listable ], [> `Non_embeddable ], [> `Non_textual ], [> `Equation_blk ]) t

val printout: Floatation.t -> Wrapper.t -> (_, _) Inline.t list option -> (_, _, _, _, [< `Source_blk ]) t ->
	([> `Manuscript ], [> `Listable ], [> `Non_embeddable ], [> `Non_textual ], [> `Printout_blk ]) t

val table: Floatation.t -> Wrapper.t -> (_, _) Inline.t list option -> (_, _, _, _, [< `Tabular_blk ]) t ->
	([> `Manuscript ], [> `Listable ], [> `Non_embeddable ], [> `Non_textual ], [> `Table_blk ]) t

val figure: Floatation.t -> Wrapper.t -> (_, _) Inline.t list option -> (_, _, _, _, [< `Subpage_blk | `Verbatim_blk | `Image_blk ]) t ->
	([> `Manuscript ], [> `Listable ], [> `Non_embeddable ], [> `Non_textual ], [> `Figure_blk ]) t

val heading: ('a, 'b, 'c, 'd, 'e) Heading.t ->
	('a, 'b, 'c, 'd, 'e) t

val title: title_level_t -> (_, _) Inline.t list ->
	([> `Manuscript ], [> `Non_listable ], [> `Non_embeddable ], [> `Non_textual ], [> `Title_blk ]) t

val abstract: (_, [< `Listable ], [< `Embeddable ], [< `Textual ], _) t list ->
	([> `Manuscript ], [> `Non_listable ], [> `Non_embeddable ], [> `Non_textual ], [> `Abstract_blk ]) t

val rule: unit ->
	([> `Manuscript ], [> `Non_listable ], [> `Non_embeddable ], [> `Non_textual ], [> `Rule_blk ]) t

val get_frag: (_, _, _, _, _) t list ->
	frag_t

