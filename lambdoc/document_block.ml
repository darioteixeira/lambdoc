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
	(**	The type of captions used in floaters.  Note that a caption
		is essentially optional inline text.
	*)
	type caption_t = Node.super_seq_t option with sexp

	(**	The possible forms for headings.  We accept three different levels
		of sections.  Note that special sections such as the TOC or the
		Bibliography are automatically mapped to the highest section level.
	*)
	type heading_t =
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

	(**	There are four different kinds of figures:
		{ul
			{li [Bitmap]: for raster images such as PNG or JPG.}
			[li [Vector]: for vectorial images such as SVG or PDF.}
			{li [Ascii]: for ASCII-art images composed of text.}
			{li [Subpage]: the image is an embedded document.}}
	*)
	type figure_t =
		[ `Bitmap of filename_t
		| `Vector of filename_t
		| `Ascii of Node.textual_seq_t
		| `Subpage of super_frag_t
		] with sexp

	(**	Top blocks may not be nested.
	*)
	type top_block_t =
		[ `Heading of heading_t
		| `Rule
		] with sexp

	(**	Nestable blocks may be nested.
	*)
	type nestable_block_t =
		[ `Paragraph of Node.super_seq_t
		| `Math of Math.t
		| `Tabular of Tabular.t
		| `Preformat of Node.textual_seq_t
		| `Itemize of Bullet.t * nestable_frag_t plus_t
		| `Enumerate of Numbering.t * nestable_frag_t plus_t
		| `Quote of Alignment.t * nestable_frag_t
		| `Algorithm of Alignment.t * Label.t * Order.floater_order_t * caption_t * Node.textual_seq_t * syntax_t
		| `Equation of Alignment.t * Label.t * Order.floater_order_t * caption_t * Math.t
		| `Figure of Alignment.t * Label.t * Order.floater_order_t * caption_t * figure_t
		| `Table of Alignment.t * Label.t * Order.floater_order_t * caption_t * Tabular.t
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
	val math: Math.t ->
		([> nestable_block_t ], [> `Composition]) t
	val tabular: Tabular.t ->
		([> nestable_block_t ], [> `Composition]) t
	val preformat: Node.textual_seq_t ->
		([> nestable_block_t ], [> `Composition]) t
	val itemize: Bullet.t -> (nestable_block_t, 'b) t list plus_t ->
		([> nestable_block_t ], 'b) t
	val enumerate: Numbering.t -> (nestable_block_t, 'b) t list plus_t ->
		([> nestable_block_t ], 'b) t
	val quote: Alignment.t -> (nestable_block_t, 'b) t list ->
		([> nestable_block_t ], 'b) t
	val algorithm: Alignment.t -> Label.t -> Order.floater_order_t -> caption_t -> Node.textual_seq_t -> syntax_t ->
		([> nestable_block_t ], [> `Manuscript]) t
	val equation: Alignment.t -> Label.t -> Order.floater_order_t -> caption_t -> Math.t ->
		([> nestable_block_t ], [> `Manuscript]) t
	val figure_bitmap: Alignment.t -> Label.t -> Order.floater_order_t -> caption_t -> filename_t ->
		([> nestable_block_t ], [> `Manuscript]) t
	val figure_vector: Alignment.t -> Label.t -> Order.floater_order_t -> caption_t -> filename_t ->
		([> nestable_block_t ], [> `Manuscript]) t
	val figure_ascii: Alignment.t -> Label.t -> Order.floater_order_t -> caption_t -> Node.textual_seq_t ->
		([> nestable_block_t ], [> `Manuscript]) t
	val figure_subpage: Alignment.t -> Label.t -> Order.floater_order_t -> caption_t -> ([< super_block_t ], 'b) t list ->
		([> nestable_block_t ], [> `Manuscript]) t
	val table: Alignment.t -> Label.t -> Order.floater_order_t -> caption_t -> Tabular.t -> 
		([> nestable_block_t ], [> `Manuscript]) t
end =
struct
	type caption_t = Node.super_seq_t option with sexp

	type heading_t =
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

	type figure_t =
		[ `Bitmap of filename_t
		| `Vector of filename_t
		| `Ascii of Node.textual_seq_t
		| `Subpage of super_frag_t
		] with sexp

	type top_block_t =
		[ `Heading of heading_t
		| `Rule
		] with sexp

	type nestable_block_t =
		[ `Paragraph of Node.super_seq_t
		| `Math of Math.t
		| `Tabular of Tabular.t
		| `Preformat of Node.textual_seq_t
		| `Itemize of Bullet.t * nestable_frag_t plus_t
		| `Enumerate of Numbering.t * nestable_frag_t plus_t
		| `Quote of Alignment.t * nestable_frag_t
		| `Algorithm of Alignment.t * Label.t * Order.floater_order_t * caption_t * Node.textual_seq_t * syntax_t
		| `Equation of Alignment.t * Label.t * Order.floater_order_t * caption_t * Math.t
		| `Figure of Alignment.t * Label.t * Order.floater_order_t * caption_t * figure_t
		| `Table of Alignment.t * Label.t * Order.floater_order_t * caption_t * Tabular.t
		] with sexp

	type super_block_t = [ top_block_t | nestable_block_t ] with sexp

	type (+'a, 'b) t = 'a constraint 'a = [< super_block_t] with sexp

	let section label order seq =
		`Heading (`Section (label, order, (seq :> Node.super_seq_t)))

	let subsection label order seq =
		`Heading (`Subsection (label, order, (seq :> Node.super_seq_t)))

	let subsubsection label order seq =
		`Heading (`Subsubsection (label, order, (seq :> Node.super_seq_t)))

	let appendix label order seq =
		`Heading (`Appendix (label, order, (seq :> Node.super_seq_t)))

	let subappendix label order seq =
		`Heading (`Subappendix (label, order, (seq :> Node.super_seq_t)))

	let subsubappendix label order seq =
		`Heading (`Subsubappendix (label, order, (seq :> Node.super_seq_t)))

	let bibliography label order =
		`Heading (`Bibliography (label, order))

	let notes label order =
		`Heading (`Notes (label, order))

	let toc label order =
		`Heading (`Toc (label, order))

	let rule () = `Rule

	let paragraph seq =
		`Paragraph (seq :> Node.super_seq_t)

	let math math =
		`Math math

	let tabular tabular_data =
		`Tabular tabular_data

	let preformat text =
		`Preformat text

	let itemize bullet (head_frag, tail_frags) =
		`Itemize (bullet, (head_frag, tail_frags))

	let enumerate numbering (head_frag, tail_frags) =
		`Enumerate (numbering, (head_frag, tail_frags))

	let quote alignment fragment =
		`Quote (alignment, fragment)

	let algorithm alignment label order caption text syntax =
		`Algorithm (alignment, label, order, caption, text, syntax)

	let equation alignment label order caption math =
		`Equation (alignment, label, order, caption, math)

	let figure_bitmap alignment label order caption filename =
		`Figure (alignment, label, order, caption, `Bitmap filename)

	let figure_vector alignment label order caption filename =
		`Figure (alignment, label, order, caption, `Vector filename)

	let figure_ascii alignment label order caption text =
		`Figure (alignment, label, order, caption, `Ascii text)

	let figure_subpage alignment label order caption frag =
		`Figure (alignment, label, order, caption, `Subpage (frag :> super_frag_t))

	let table alignment label order caption tabular_data =
		`Table (alignment, label, order, caption, tabular_data)
end

