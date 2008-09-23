(********************************************************************************)
(**	Definition of document blocks.  Blocks are the higher-level structures
	found in a document.  They include paragraphs, figures, tables, etc.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

TYPE_CONV_PATH "Document"

open Document_basic
open Document_node
open Document_ref
open Document_tabular
open Document_math


(********************************************************************************)
(*	{2 Bullet module}							*)
(********************************************************************************)

(**	The various sorts of bullets accepted for unordered lists.
	Note that these map directly into their CSS counterparts.
*)
module Bullet =
struct
	exception Unknown_bullet_type of string

	type t =
		| Default
		| Disc
		| Circle
		| Square
		| None
		with sexp

	let of_string = function
		| "disc"	-> Disc
		| "circle"	-> Circle
		| "square"	-> Square
		| "none"	-> None
		| other		-> raise (Unknown_bullet_type other)

	let to_string = function
		| Default
		| Disc		-> "disc"
		| Circle	-> "circle"
		| Square	-> "square"
		| None		-> "none"
end


(********************************************************************************)
(**	{2 Numbering module}							*)
(********************************************************************************)

(**	The various sorts of numbering accepted for ordered lists.
	Note that these map directly into their CSS counterparts.
*)
module Numbering =
struct
	exception Unknown_numbering_type of string

	type t =
		| Default
		| Decimal
		| Lower_roman
		| Upper_roman
		| Lower_alpha
		| Upper_alpha
		| None
		with sexp

	let of_string = function
		| "0"		-> Decimal
		| "i"		-> Lower_roman
		| "I"		-> Upper_roman
		| "a"		-> Lower_alpha
		| "A"		-> Upper_alpha
		| "none"	-> None
		| other		-> raise (Unknown_numbering_type other)

	let to_string = function
		| Default
		| Decimal	-> "decimal"
		| Lower_roman	-> "lower_roman"
		| Upper_roman	-> "upper_roman"
		| Lower_alpha	-> "lower_alpha"
		| Upper_alpha	-> "upper_alpha"
		| None		-> "none"
end


(********************************************************************************)
(**	{2 Alignment module}							*)
(********************************************************************************)

module Alignment =
struct
	exception Unknown_alignment_type of string

	type t =
		| Center
		| Left
		| Right with sexp

	let of_string = function
		| "center"	-> Center
		| "left"	-> Left
		| "right"	-> Right
		| other		-> raise (Unknown_alignment_type other)

	let to_string = function
		| Center	-> "center"
		| Left		-> "left"
		| Right		-> "right"
end


(********************************************************************************)
(**	{2 Block module}							*)
(********************************************************************************)

(**	A visible block is one which always appears in the document in the same
	order as it was defined.
*)
module rec Block:
sig
	(**	The tuple of all common fields to floaters.  These
		are the floater's label, its order, and a caption.
	*)
	type floater_t = Label.t * Order.floater_order_t * Node.super_seq_t with sexp

	(**	The possible forms for headings.  We accept three different levels
		of sections.  Note that special sections such as the TOC or the
		Bibliography are automatically mapped to the highest section level.
	*)
	type heading_block_t =
		[ `Section of Label.t * Order.user_sectional_order_t * Node.super_seq_t
		| `Subsection of Label.t * Order.user_sectional_order_t * Node.super_seq_t
		| `Subsubsection of Label.t * Order.user_sectional_order_t * Node.super_seq_t
		| `Appendix of Label.t * Order.user_sectional_order_t * Node.super_seq_t
		| `Subappendix of Label.t * Order.user_sectional_order_t * Node.super_seq_t
		| `Subsubappendix of Label.t * Order.user_sectional_order_t * Node.super_seq_t
		| `Bibliography of Label.t * Order.preset_sectional_order_t
		| `Notes of Label.t * Order.preset_sectional_order_t
		| `Toc of Label.t * Order.preset_sectional_order_t
		] with sexp

	(**	A list of super blocks.
	*)
	type super_frag_t = Block.super_block_t list with sexp

	(**	A list of nestable blocks.
	*)
	type nestable_frag_t = Block.nestable_block_t list with sexp

	(**	Math block.
	*)
	type math_block_t = [ `Math of Alignment.t * Math.t ] with sexp 

	(**	Code block.
	*)
	type code_block_t = [ `Code of Alignment.t * syntax_t * Node.textual_seq_t ] with sexp 

	(**	Tabular block.
	*)
	type tabular_block_t = [ `Tabular of Alignment.t * Tabular.t ] with sexp 

	(**	Image block.
	*)
	type image_block_t = [ `Image of Alignment.t * alias_t ] with sexp 

	(**	Verbatim block.
	*)
	type verbatim_block_t = [ `Verbatim of Alignment.t * Node.textual_seq_t ] with sexp 

	(**	Subpage block.
	*)
	type subpage_block_t = [ `Subpage of Alignment.t * super_frag_t ] with sexp 

	(**	The type of equations.  Equations are just a floater wrapper
		around a math block.
	*)
	type equation_block_t = [ math_block_t ] with sexp

	(**	The type of algorithms.  Algorithms are just a floater wrapper
		around a code block.
	*)
	type algorithm_block_t = [ code_block_t ] with sexp

	(**	The type of tables.  Tables are just a floater wrapper
		around a tabular block.
	*)
	type table_block_t = [ tabular_block_t ] with sexp

	(**	The type of figures.  Figures are just a floater wrapper
		around one of the three figure types: images, verbatim
		environments (for ASCII-art), and subpages.
	*)
	type figure_block_t = [ image_block_t | verbatim_block_t | subpage_block_t ] with sexp

	(**	Top blocks may not be nested.
	*)
	type top_block_t =
		[ `Heading of heading_block_t
		| `Rule
		] with sexp

	(**	Nestable blocks may be nested.
	*)
	type nestable_block_t =
		[ `Paragraph of Node.super_seq_t
		| `Itemize of Bullet.t * nestable_frag_t plus_t
		| `Enumerate of Numbering.t * nestable_frag_t plus_t
		| `Quote of Alignment.t * nestable_frag_t
		| math_block_t
		| code_block_t
		| verbatim_block_t
		| tabular_block_t
		| image_block_t
		| subpage_block_t
		| `Equation of floater_t * equation_block_t
		| `Algorithm of floater_t * algorithm_block_t
		| `Table of floater_t * table_block_t
		| `Figure of floater_t * figure_block_t
		] with sexp

	type super_block_t = [ top_block_t | nestable_block_t ] with sexp

	type (+'a, 'b) t = 'a constraint 'a = [< super_block_t] with sexp

	val section: Label.t -> Order.user_sectional_order_t -> ([< Node.super_node_t ], 'b) Node.t list ->
		([> top_block_t ], [> `Manuscript ]) t
	val subsection: Label.t -> Order.user_sectional_order_t -> ([< Node.super_node_t ], 'b) Node.t list ->
		([> top_block_t ], [> `Manuscript ]) t
	val subsubsection: Label.t -> Order.user_sectional_order_t -> ([< Node.super_node_t ], 'b) Node.t list ->
		([> top_block_t ], [> `Manuscript ]) t
	val appendix: Label.t -> Order.user_sectional_order_t -> ([< Node.super_node_t ], 'b) Node.t list ->
		([> top_block_t ], [> `Manuscript ]) t
	val subappendix: Label.t -> Order.user_sectional_order_t -> ([< Node.super_node_t ], 'b) Node.t list ->
		([> top_block_t ], [> `Manuscript ]) t
	val subsubappendix: Label.t -> Order.user_sectional_order_t -> ([< Node.super_node_t ], 'b) Node.t list ->
		([> top_block_t ], [> `Manuscript ]) t
	val bibliography: Label.t -> Order.preset_sectional_order_t ->
		([> top_block_t ], [> `Manuscript ]) t
	val notes: Label.t -> Order.preset_sectional_order_t ->
		([> top_block_t ], [> `Manuscript ]) t
	val toc: Label.t -> Order.preset_sectional_order_t ->
		([> top_block_t ], [> `Manuscript ]) t

	val rule: unit ->
		([> top_block_t ], [> `Manuscript ]) t

	val paragraph: ([< Node.super_node_t ], 'b) Node.t list ->
		([> nestable_block_t ], 'b) t
	val itemize: Bullet.t -> (nestable_block_t, 'b) t list plus_t ->
		([> nestable_block_t ], 'b) t
	val enumerate: Numbering.t -> (nestable_block_t, 'b) t list plus_t ->
		([> nestable_block_t ], 'b) t
	val quote: Alignment.t -> (nestable_block_t, 'b) t list ->
		([> nestable_block_t ], 'b) t
	val math: Alignment.t -> Math.t ->
		([> math_block_t ], [> `Composition]) t
	val code: Alignment.t -> syntax_t -> Node.textual_seq_t ->
		([> code_block_t ], [> `Composition]) t
	val verbatim: Alignment.t -> Node.textual_seq_t ->
		([> verbatim_block_t ], [> `Composition]) t
	val tabular: Alignment.t -> Tabular.t ->
		([> tabular_block_t ], [> `Manuscript]) t
	val image: Alignment.t -> alias_t ->
		([> image_block_t ], [> `Composition]) t
	val subpage: Alignment.t -> ([< super_block_t ], 'b) t list ->
		([> subpage_block_t ], 'b) t

	val equation: floater_t -> equation_block_t ->
		([> nestable_block_t ], [> `Manuscript]) t
	val algorithm: floater_t -> algorithm_block_t ->
		([> nestable_block_t ], [> `Manuscript]) t
	val table: floater_t -> table_block_t ->
		([> nestable_block_t ], [> `Manuscript]) t
	val figure: floater_t -> figure_block_t ->
		([> nestable_block_t ], [> `Manuscript]) t
end =
struct
	type floater_t = Label.t * Order.floater_order_t * Node.super_seq_t with sexp

	type heading_block_t =
		[ `Section of Label.t * Order.user_sectional_order_t * Node.super_seq_t
		| `Subsection of Label.t * Order.user_sectional_order_t * Node.super_seq_t
		| `Subsubsection of Label.t * Order.user_sectional_order_t * Node.super_seq_t
		| `Appendix of Label.t * Order.user_sectional_order_t * Node.super_seq_t
		| `Subappendix of Label.t * Order.user_sectional_order_t * Node.super_seq_t
		| `Subsubappendix of Label.t * Order.user_sectional_order_t * Node.super_seq_t
		| `Bibliography of Label.t * Order.preset_sectional_order_t
		| `Notes of Label.t * Order.preset_sectional_order_t
		| `Toc of Label.t * Order.preset_sectional_order_t
		] with sexp

	type super_frag_t = Block.super_block_t list with sexp

	type nestable_frag_t = Block.nestable_block_t list with sexp

	type math_block_t = [ `Math of Alignment.t * Math.t ] with sexp 

	type code_block_t = [ `Code of Alignment.t * syntax_t * Node.textual_seq_t ] with sexp 

	type tabular_block_t = [ `Tabular of Alignment.t * Tabular.t ] with sexp 

	type image_block_t = [ `Image of Alignment.t * alias_t ] with sexp 

	type verbatim_block_t = [ `Verbatim of Alignment.t * Node.textual_seq_t ] with sexp 

	type subpage_block_t = [ `Subpage of Alignment.t * super_frag_t ] with sexp 

	type equation_block_t = [ math_block_t ] with sexp

	type algorithm_block_t = [ code_block_t ] with sexp

	type table_block_t = [ tabular_block_t ] with sexp

	type figure_block_t = [ image_block_t | verbatim_block_t | subpage_block_t ] with sexp

	type top_block_t =
		[ `Heading of heading_block_t
		| `Rule
		] with sexp

	type nestable_block_t =
		[ `Paragraph of Node.super_seq_t
		| `Itemize of Bullet.t * nestable_frag_t plus_t
		| `Enumerate of Numbering.t * nestable_frag_t plus_t
		| `Quote of Alignment.t * nestable_frag_t
		| math_block_t
		| code_block_t
		| verbatim_block_t
		| tabular_block_t
		| image_block_t
		| subpage_block_t
		| `Equation of floater_t * equation_block_t
		| `Algorithm of floater_t * algorithm_block_t
		| `Table of floater_t * table_block_t
		| `Figure of floater_t * figure_block_t
		] with sexp

	type super_block_t = [ top_block_t | nestable_block_t ] with sexp

	type (+'a, 'b) t = 'a constraint 'a = [< super_block_t] with sexp

	let section label order seq = `Heading (`Section (label, order, (seq :> Node.super_seq_t)))
	let subsection label order seq = `Heading (`Subsection (label, order, (seq :> Node.super_seq_t)))
	let subsubsection label order seq = `Heading (`Subsubsection (label, order, (seq :> Node.super_seq_t)))
	let appendix label order seq = `Heading (`Appendix (label, order, (seq :> Node.super_seq_t)))
	let subappendix label order seq = `Heading (`Subappendix (label, order, (seq :> Node.super_seq_t)))
	let subsubappendix label order seq = `Heading (`Subsubappendix (label, order, (seq :> Node.super_seq_t)))
	let bibliography label order = `Heading (`Bibliography (label, order))
	let notes label order = `Heading (`Notes (label, order))
	let toc label order = `Heading (`Toc (label, order))

	let rule () = `Rule

	let paragraph seq = `Paragraph (seq :> Node.super_seq_t)
	let itemize bullet (head_frag, tail_frags) = `Itemize (bullet, (head_frag, tail_frags))
	let enumerate numbering (head_frag, tail_frags) = `Enumerate (numbering, (head_frag, tail_frags))
	let quote alignment frag = `Quote (alignment, frag)
	let math alignment math = `Math (alignment, math)
	let code alignment syntax txt = `Code (alignment, syntax, txt)
	let verbatim alignment txt = `Verbatim (alignment, txt)
	let tabular alignment tab = `Tabular (alignment, tab)
	let image alignment alias = `Image (alignment, alias)
	let subpage alignment frag = `Subpage (alignment, (frag :> super_block_t list))

	let equation (label, order, caption) math = `Equation ((label, order, caption), math)
	let algorithm (label, order, caption) code = `Algorithm ((label, order, caption), code)
	let table (label, order, caption) tabular = `Table ((label, order, caption), tabular)
	let figure (label, order, caption) figure = `Figure ((label, order, caption), figure)
end

