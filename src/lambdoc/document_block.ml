(********************************************************************************)
(*	Implementation file for Document_block.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of document blocks.  Blocks are the higher-level structures
	found in a document.  They include paragraphs, figures, tables, etc.
*)

TYPE_CONV_PATH "Document"

open Document_basic
open Document_node
open Document_ref
open Document_math
open Document_code
open Document_tabular


(********************************************************************************)
(*	{2 Bullet module}							*)
(********************************************************************************)

(**	The various sorts of bullets accepted for unordered lists.
	Note that these map directly into their CSS counterparts.
*)
module Bullet =
struct
	exception Unknown_bullet of string

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
		| other		-> raise (Unknown_bullet other)

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
	exception Unknown_numbering of string

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
		| other		-> raise (Unknown_numbering other)

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
	exception Unknown_alignment of string

	type t =
		| Center
		| Left
		| Right with sexp

	let of_string = function
		| "center"	-> Center
		| "left"	-> Left
		| "right"	-> Right
		| other		-> raise (Unknown_alignment other)

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
	(**	A super fragment is a list of super blocks.
	*)
	type super_frag_t = Block.super_block_t list with sexp

	(**	A nestable fragment is a list of nestable blocks.
	*)
	type nestable_frag_t = Block.nestable_block_t list with sexp

	(**	The various types of individual nestable blocks.
	*)

	type paragraph_block_t = [ `Paragraph of Node.super_seq_t ] with sexp
	type itemize_block_t = [ `Itemize of Bullet.t * nestable_frag_t plus_t ] with sexp
	type enumerate_block_t = [ `Enumerate of Numbering.t * nestable_frag_t plus_t ] with sexp
	type quote_block_t = [ `Quote of Alignment.t * nestable_frag_t ] with sexp
	type math_block_t = [ `Math of Alignment.t * Math.t ] with sexp 
	type code_block_t = [ `Code of Alignment.t * Code.t ] with sexp 
	type tabular_block_t = [ `Tabular of Alignment.t * Tabular.t ] with sexp 
	type image_block_t = [ `Image of Alignment.t * alias_t ] with sexp 
	type verbatim_block_t = [ `Verbatim of Alignment.t * raw_t ] with sexp 
	type subpage_block_t = [ `Subpage of Alignment.t * super_frag_t ] with sexp 

	(**	The various types of wrapper blocks.  Wrappers are just
		numbered containers around some kinds of nestable blocks.
	*)

	type equation_block_t = [ math_block_t ] with sexp
	type algorithm_block_t = [ code_block_t ] with sexp
	type table_block_t = [ tabular_block_t ] with sexp
	type figure_block_t = [ image_block_t | verbatim_block_t | subpage_block_t ] with sexp

	(**	The tuple of all common fields to wrappers.  The fields
		are the wrapper's label, its order, and a caption.
	*)
	type wrapper_t = Label.t * Order.wrapper_order_t * Node.super_seq_t with sexp

	(**	Nestable blocks may be nested.
	*)
	type nestable_block_t =
		[ paragraph_block_t
		| itemize_block_t
		| enumerate_block_t
		| quote_block_t
		| math_block_t
		| code_block_t
		| verbatim_block_t
		| tabular_block_t
		| image_block_t
		| subpage_block_t
		| `Equation of wrapper_t * equation_block_t
		| `Algorithm of wrapper_t * algorithm_block_t
		| `Table of wrapper_t * table_block_t
		| `Figure of wrapper_t * figure_block_t
		] with sexp

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

	(**	Top blocks may not be nested.
	*)
	type top_block_t =
		[ `Heading of heading_block_t
		| `Title of Node.super_seq_t
		| `Subtitle of Node.super_seq_t
		| `Abstract of paragraph_block_t list
		| `Rule
		] with sexp

	(**	A super block is either a top block or a nestable block.
	*)
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

	val title: ([< Node.super_node_t ], 'b) Node.t list ->
		([> top_block_t ], [> `Manuscript ]) t
	val subtitle: ([< Node.super_node_t ], 'b) Node.t list ->
		([> top_block_t ], [> `Manuscript ]) t
	val abstract: (paragraph_block_t, 'b) t list ->
		([> top_block_t ], [> `Manuscript ]) t
	val rule: unit ->
		([> top_block_t ], [> `Manuscript ]) t

	val paragraph: ([< Node.super_node_t ], 'b) Node.t list ->
		([> paragraph_block_t ], 'b) t
	val itemize: Bullet.t -> (nestable_block_t, 'b) t list plus_t ->
		([> itemize_block_t ], 'b) t
	val enumerate: Numbering.t -> (nestable_block_t, 'b) t list plus_t ->
		([> enumerate_block_t ], 'b) t
	val quote: Alignment.t -> (nestable_block_t, 'b) t list ->
		([> quote_block_t ], 'b) t
	val math: Alignment.t -> Math.t ->
		([> math_block_t ], [> `Composition]) t
	val code: Alignment.t -> Code.t ->
		([> code_block_t ], [> `Composition]) t
	val verbatim: Alignment.t -> raw_t ->
		([> verbatim_block_t ], [> `Composition]) t
	val tabular: Alignment.t -> Tabular.t ->
		([> tabular_block_t ], [> `Manuscript]) t
	val image: Alignment.t -> alias_t ->
		([> image_block_t ], [> `Composition]) t
	val subpage: Alignment.t -> ([< super_block_t ], 'b) t list ->
		([> subpage_block_t ], 'b) t

	val equation: wrapper_t -> equation_block_t ->
		([> nestable_block_t ], [> `Manuscript]) t
	val algorithm: wrapper_t -> algorithm_block_t ->
		([> nestable_block_t ], [> `Manuscript]) t
	val table: wrapper_t -> table_block_t ->
		([> nestable_block_t ], [> `Manuscript]) t
	val figure: wrapper_t -> figure_block_t ->
		([> nestable_block_t ], [> `Manuscript]) t
end =
struct
	type super_frag_t = Block.super_block_t list with sexp

	type nestable_frag_t = Block.nestable_block_t list with sexp

	type paragraph_block_t = [ `Paragraph of Node.super_seq_t ] with sexp
	type itemize_block_t = [ `Itemize of Bullet.t * nestable_frag_t plus_t ] with sexp
	type enumerate_block_t = [ `Enumerate of Numbering.t * nestable_frag_t plus_t ] with sexp
	type quote_block_t = [ `Quote of Alignment.t * nestable_frag_t ] with sexp
	type math_block_t = [ `Math of Alignment.t * Math.t ] with sexp 
	type code_block_t = [ `Code of Alignment.t * Code.t ] with sexp 
	type tabular_block_t = [ `Tabular of Alignment.t * Tabular.t ] with sexp 
	type image_block_t = [ `Image of Alignment.t * alias_t ] with sexp 
	type verbatim_block_t = [ `Verbatim of Alignment.t * raw_t ] with sexp 
	type subpage_block_t = [ `Subpage of Alignment.t * super_frag_t ] with sexp 

	type equation_block_t = [ math_block_t ] with sexp
	type algorithm_block_t = [ code_block_t ] with sexp
	type table_block_t = [ tabular_block_t ] with sexp
	type figure_block_t = [ image_block_t | verbatim_block_t | subpage_block_t ] with sexp

	type wrapper_t = Label.t * Order.wrapper_order_t * Node.super_seq_t with sexp

	type nestable_block_t =
		[ paragraph_block_t
		| itemize_block_t
		| enumerate_block_t
		| quote_block_t
		| math_block_t
		| code_block_t
		| verbatim_block_t
		| tabular_block_t
		| image_block_t
		| subpage_block_t
		| `Equation of wrapper_t * equation_block_t
		| `Algorithm of wrapper_t * algorithm_block_t
		| `Table of wrapper_t * table_block_t
		| `Figure of wrapper_t * figure_block_t
		] with sexp

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

	type top_block_t =
		[ `Heading of heading_block_t
		| `Title of Node.super_seq_t
		| `Subtitle of Node.super_seq_t
		| `Abstract of paragraph_block_t list
		| `Rule
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

	let title seq = `Title (seq :> Node.super_seq_t)
	let subtitle seq = `Subtitle (seq :> Node.super_seq_t)
	let abstract frag = `Abstract frag
	let rule () = `Rule

	let paragraph seq = `Paragraph (seq :> Node.super_seq_t)
	let itemize bullet (head_frag, tail_frags) = `Itemize (bullet, (head_frag, tail_frags))
	let enumerate numbering (head_frag, tail_frags) = `Enumerate (numbering, (head_frag, tail_frags))
	let quote alignment frag = `Quote (alignment, frag)
	let math alignment math = `Math (alignment, math)
	let code alignment highlight = `Code (alignment, highlight)
	let verbatim alignment txt = `Verbatim (alignment, txt)
	let tabular alignment tab = `Tabular (alignment, tab)
	let image alignment alias = `Image (alignment, alias)
	let subpage alignment frag = `Subpage (alignment, (frag :> super_block_t list))

	let equation (label, order, caption) math = `Equation ((label, order, caption), math)
	let algorithm (label, order, caption) code = `Algorithm ((label, order, caption), code)
	let table (label, order, caption) tabular = `Table ((label, order, caption), tabular)
	let figure (label, order, caption) figure = `Figure ((label, order, caption), figure)
end

