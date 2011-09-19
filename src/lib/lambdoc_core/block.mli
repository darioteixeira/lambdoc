(********************************************************************************)
(*	Block.mli
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning block elements.
*)

open Prelude
open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type 'a block_t =
	[ `Paragraph of bool * bool option * Inline.seq_t
	| `Itemize of Bullet.t * 'a nelist nelist
	| `Enumerate of Numbering.t * 'a nelist nelist
	| `Description of (Inline.seq_t * 'a nelist) nelist
	| `Qanda of ((Inline.seq_t option option * 'a nelist) * (Inline.seq_t option option * 'a nelist)) nelist
	| `Verse of 'a nelist
	| `Quote of 'a nelist
	| `Math of Math.t
	| `Source of Source.t
	| `Tabular of Tabular.tabular_t
	| `Subpage of 'a nelist
	| `Verbatim of int * string
	| `Picture of bool * int option * Alias.t * string
	| `Bookpic of Book.isbn_t * Book.rating_t option * Book.cover_t
	| `Decor of Floatation.t * 'a
	| `Pullquote of Floatation.t * Inline.seq_t option * 'a nelist
	| `Boxout of Floatation.t * Custom.Boxout.t * Inline.seq_t option * 'a nelist
	| `Theorem of Custom.Theorem.t * Inline.seq_t option * 'a nelist
	| `Equation of Floatation.t * Wrapper.t * 'a
	| `Printout of Floatation.t * Wrapper.t * 'a
	| `Table of Floatation.t * Wrapper.t * 'a
	| `Figure of Floatation.t * Wrapper.t * 'a
	| `Heading of Heading.heading_t
	| `Title of Level.title_t * Inline.seq_t
	| `Abstract of 'a nelist
	| `Rule
	] with sexp

type raw_block_t = raw_block_t block_t with sexp
type frag_t = raw_block_t nelist with sexp

type (+'a, +'b, +'c, +'d, +'e) t = private [< ('a, 'b, 'c, 'd, 'e) t block_t ] with sexp


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

(**	The functions in this module use phantom types to enforce some invariants
	on block nesting.  The main type {!t} has five of these phantom types:
	{ul	{li ['a] is either [`Composition] or [`Manuscript].  The former
		does not allow for elements that may produce internal links in
		the document, whereas the latter allows everything.}
		{li ['b] is either [`Listable] or [`Non_listable].  A block is
		listable if it is a wrapper (equation, printout, table, or figure),
		a pullquote, a boxout, a custom environment, or embeddable.}
		{li ['c] is either [`Quotable] or [`Non_quotable].  A block
		is termed quotable if it can be a child of quotations.  Note
		that all textual blocks are also quotable.}
		{li ['d] is either [`Embeddable] or [`Non_embeddable].  A block is
		defined as embeddable if it's one the basic block types that may
		be embedded almost anywhere.
		{li ['e] indicates the actual block type.}}
*)

val paragraph: bool -> bool option -> ('a, _) Inline.t nelist ->
	('a, [> `Listable ], [> `Quotable ], [> `Embeddable ], [> `Paragraph_blk ]) t

val itemize: Bullet.t -> ('a, [< `Listable ], 'c, 'd, _) t nelist nelist ->
	('a, [> `Listable ], 'c, 'd, [> `Itemize_blk ]) t

val enumerate: Numbering.t -> ('a, [< `Listable ], 'c, 'd, _) t nelist nelist ->
	('a, [> `Listable ], 'c, 'd, [> `Enumerate_blk ]) t

val description: (('a, _) Inline.t nelist * ('a, [< `Listable ], 'c, 'd, _) t nelist) nelist ->
	('a, [> `Listable ], 'c, 'd, [> `Description_blk ]) t

val qanda: ((('a, _) Inline.t nelist option option * ('a, [< `Listable ], 'c, 'd, _) t nelist) * (('a, _) Inline.t nelist option option * ('a, [< `Listable ], 'c, 'd, _) t nelist)) nelist ->
	('a, [> `Listable ], 'c, 'd, [> `Qanda_blk ]) t

val verse: ('a, _, _, _, [< `Paragraph_block ]) t nelist ->
	('a, [> `Listable ], [> `Quotable ], [> `Embeddable], [> `Verse_blk ]) t

val quote: ('a, [< `Listable ], [< `Quotable ], _, _) t nelist ->
	('a, [> `Listable ], [> `Quotable], [> `Non_embeddable ], [> `Quote_blk ]) t

val math: Math.t ->
	([> `Composition ], [> `Listable ], [> `Quotable ], [> `Embeddable ], [> `Math_blk ]) t

val source: Source.t ->
	([> `Composition ], [> `Listable ], [> `Quotable ], [> `Embeddable ], [> `Source_blk ]) t

val tabular: 'a Tabular.t ->
	('a, [> `Listable ], [> `Quotable ], [> `Embeddable ], [> `Tabular_blk ]) t

val subpage: ('a, _, _, _, _) t nelist ->
	('a, [> `Listable ], [> `Non_quotable ], [> `Non_embeddable ], [> `Subpage_blk ]) t

val verbatim: int -> string ->
	([> `Composition ], [> `Listable ], [> `Quotable], [> `Embeddable ], [> `Verbatim_blk ]) t

val picture: bool -> int option -> Alias.t -> string ->
	([> `Composition ], [> `Listable ], [> `Quotable ], [> `Embeddable ], [> `Picture_blk ]) t

val bookpic: Book.isbn_t -> Book.rating_t option -> Book.cover_t ->
	([> `Composition ], [> `Listable ], [> `Quotable ], [> `Embeddable ], [> `Bookcover_blk ]) t

val decor: Floatation.t -> (_, _, _, _, [< `Verbatim_blk | `Picture_blk | `Bookcover_blk ]) t ->
	([> `Manuscript ], [> `Listable ], [> `Quotable], [> `Embeddable ], [> `Decor_blk ]) t

val pullquote: Floatation.t -> (_, _) Inline.t nelist option -> (_, [< `Listable ], [< `Quotable ], [< `Embeddable ], _) t nelist ->
	([> `Manuscript ], [> `Listable ], [> `Non_quotable], [> `Non_embeddable ], [> `Pullquote_blk ]) t

val boxout: Floatation.t -> Custom.Boxout.t -> (_, _) Inline.t nelist option -> (_, [< `Listable ], [< `Quotable ], _, _) t nelist ->
	([> `Manuscript ], [> `Listable ], [> `Non_quotable ], [> `Non_embeddable ], [> `Boxout_blk ]) t

val theorem: Custom.Theorem.t -> (_, _) Inline.t nelist option -> (_, [< `Listable ], [< `Quotable ], [< `Embeddable ], _) t nelist ->
	([> `Manuscript ], [> `Listable ], [> `Non_quotable ], [> `Non_embeddable ], [> `Theorem_blk ]) t

val equation: Floatation.t -> Wrapper.t -> (_, _, _, _, [< `Mathblk_blk ]) t ->
	([> `Manuscript ], [> `Listable ], [> `Non_quotable ], [> `Non_embeddable ], [> `Equation_blk ]) t

val printout: Floatation.t -> Wrapper.t -> (_, _, _, _, [< `Source_blk ]) t ->
	([> `Manuscript ], [> `Listable ], [> `Non_quotable ], [> `Non_embeddable ], [> `Printout_blk ]) t

val table: Floatation.t -> Wrapper.t -> (_, _, _, _, [< `Tabular_blk ]) t ->
	([> `Manuscript ], [> `Listable ], [> `Non_quotable ], [> `Non_embeddable ], [> `Table_blk ]) t

val figure: Floatation.t -> Wrapper.t -> (_, _, _, _, [< `Subpage_blk | `Verbatim_blk | `Picture_blk ]) t ->
	([> `Manuscript ], [> `Listable ], [> `Non_quotable ], [> `Non_embeddable ], [> `Figure_blk ]) t

val heading: ('a, 'b, 'c, 'd, 'e) Heading.t ->
	('a, 'b, 'c, 'd, 'e) t

val title: Level.title_t -> (_, _) Inline.t nelist ->
	([> `Manuscript ], [> `Non_listable ], [> `Non_quotable ], [> `Non_embeddable ], [> `Title_blk ]) t

val abstract: (_, [< `Listable ], [< `Quotable ], [< `Embeddable ], _) t nelist ->
	([> `Manuscript ], [> `Non_listable ], [> `Non_quotable ], [> `Non_embeddable ], [> `Abstract_blk ]) t

val rule: unit ->
	([> `Manuscript ], [> `Non_listable ], [> `Non_quotable ], [> `Non_embeddable ], [> `Rule_blk ]) t

val get_frag: (_, _, _, _, _) t nelist ->
	frag_t

val get_blocks: (_, _, _, _, _) t list ->
	raw_block_t list

