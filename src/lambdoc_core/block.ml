(********************************************************************************)
(*	Implementation file for Block module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of document blocks.  Blocks are the higher-level structures
	found in a document.  They include paragraphs, figures, tables, etc.
*)

TYPE_CONV_PATH "Document"

open Basic


module rec M:
sig
	(**	A super fragment is a list of super blocks.
	*)
	type super_frag_t = M.super_block_t list (*with sexp*)


	(**	A nestable fragment is a list of nestable blocks.
	*)
	type nestable_frag_t = M.nestable_block_t list (*with sexp*)


	(**	The various types of individual nestable blocks.
	*)

	type paragraph_block_t = [ `Paragraph of Node.M.super_seq_t ] (*with sexp*)
	type itemize_block_t = [ `Itemize of Bullet.t * nestable_frag_t plus_t ] (*with sexp*)
	type enumerate_block_t = [ `Enumerate of Numbering.t * nestable_frag_t plus_t ] (*with sexp*)
	type quote_block_t = [ `Quote of Alignment.t * nestable_frag_t ] (*with sexp*)
	type math_block_t = [ `Math of Alignment.t * Math.t ] (*with sexp*) 
	type code_block_t = [ `Code of Alignment.t * Code.t ] (*with sexp*) 
	type tabular_block_t = [ `Tabular of Alignment.t * Tabular.t ] (*with sexp*) 
	type image_block_t = [ `Image of Alignment.t * alias_t ] (*with sexp*) 
	type verbatim_block_t = [ `Verbatim of Alignment.t * raw_t ] (*with sexp*) 
	type subpage_block_t = [ `Subpage of Alignment.t * super_frag_t ] (*with sexp*) 


	(**	The various types of wrapper blocks.  Wrappers are just
		numbered containers around some kinds of nestable blocks.
	*)

	type equation_block_t = [ math_block_t ] (*with sexp*)
	type algorithm_block_t = [ code_block_t ] (*with sexp*)
	type table_block_t = [ tabular_block_t ] (*with sexp*)
	type figure_block_t = [ image_block_t | verbatim_block_t | subpage_block_t ] (*with sexp*)


	(**	The tuple of all common fields to wrappers.  The fields
		are the wrapper's label, its order, and a caption.
	*)
	type wrapper_t = Label.t * Order.wrapper_order_t * Node.M.super_seq_t (*with sexp*)


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
		] (*with sexp*)


	(**	The possible forms for headings.  Besides main body sections, we accept
		also appendix sections.  Note that special sections such as the TOC or	
		the Bibliography are automatically mapped to the highest hierarchy level.
	*)
	type heading_block_t =
		[ `Section of Level.t * Label.t * Order.sectional_order_t * Node.M.super_seq_t
		| `Appendix of Level.t * Label.t * Order.appendix_order_t * Node.M.super_seq_t
		| `Bibliography of Label.t * Order.preset_order_t
		| `Notes of Label.t * Order.preset_order_t
		| `Toc of Label.t * Order.preset_order_t
		] (*with sexp*)


	(**	Top blocks may not be nested.
	*)
	type top_block_t =
		[ `Heading of heading_block_t
		| `Title of Node.M.super_seq_t
		| `Subtitle of Node.M.super_seq_t
		| `Abstract of paragraph_block_t list
		| `Part of Node.M.super_seq_t
		| `Rule
		] (*with sexp*)


	(**	A super block is either a top block or a nestable block.
	*)
	type super_block_t = [ top_block_t | nestable_block_t ] (*with sexp*)

	type (+'a, 'b) t = 'a constraint 'a = [< super_block_t] (*with sexp*)

	val section: Level.t -> Label.t -> Order.sectional_order_t -> ([< Node.M.super_node_t ], 'b) Node.M.t list ->
		([> top_block_t ], [> `Manuscript ]) t
	val appendix: Level.t -> Label.t -> Order.appendix_order_t -> ([< Node.M.super_node_t ], 'b) Node.M.t list ->
		([> top_block_t ], [> `Manuscript ]) t
	val bibliography: Label.t -> Order.preset_order_t ->
		([> top_block_t ], [> `Manuscript ]) t
	val notes: Label.t -> Order.preset_order_t ->
		([> top_block_t ], [> `Manuscript ]) t
	val toc: Label.t -> Order.preset_order_t ->
		([> top_block_t ], [> `Manuscript ]) t

	val title: ([< Node.M.super_node_t ], 'b) Node.M.t list ->
		([> top_block_t ], [> `Manuscript ]) t
	val subtitle: ([< Node.M.super_node_t ], 'b) Node.M.t list ->
		([> top_block_t ], [> `Manuscript ]) t
	val abstract: (paragraph_block_t, 'b) t list ->
		([> top_block_t ], [> `Manuscript ]) t
	val part: ([< Node.M.super_node_t ], 'b) Node.M.t list ->
		([> top_block_t ], [> `Manuscript ]) t
	val rule: unit ->
		([> top_block_t ], [> `Manuscript ]) t

	val paragraph: ([< Node.M.super_node_t ], 'b) Node.M.t list ->
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
	type super_frag_t = M.super_block_t list (*with sexp*)

	type nestable_frag_t = M.nestable_block_t list (*with sexp*)

	type paragraph_block_t = [ `Paragraph of Node.M.super_seq_t ] (*with sexp*)
	type itemize_block_t = [ `Itemize of Bullet.t * nestable_frag_t plus_t ] (*with sexp*)
	type enumerate_block_t = [ `Enumerate of Numbering.t * nestable_frag_t plus_t ] (*with sexp*)
	type quote_block_t = [ `Quote of Alignment.t * nestable_frag_t ] (*with sexp*)
	type math_block_t = [ `Math of Alignment.t * Math.t ] (*with sexp*) 
	type code_block_t = [ `Code of Alignment.t * Code.t ] (*with sexp*) 
	type tabular_block_t = [ `Tabular of Alignment.t * Tabular.t ] (*with sexp*) 
	type image_block_t = [ `Image of Alignment.t * alias_t ] (*with sexp*) 
	type verbatim_block_t = [ `Verbatim of Alignment.t * raw_t ] (*with sexp*) 
	type subpage_block_t = [ `Subpage of Alignment.t * super_frag_t ] (*with sexp*) 

	type equation_block_t = [ math_block_t ] (*with sexp*)
	type algorithm_block_t = [ code_block_t ] (*with sexp*)
	type table_block_t = [ tabular_block_t ] (*with sexp*)
	type figure_block_t = [ image_block_t | verbatim_block_t | subpage_block_t ] (*with sexp*)

	type wrapper_t = Label.t * Order.wrapper_order_t * Node.M.super_seq_t (*with sexp*)

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
		] (*with sexp*)

	type heading_block_t =
		[ `Section of Level.t * Label.t * Order.sectional_order_t * Node.M.super_seq_t
		| `Appendix of Level.t * Label.t * Order.appendix_order_t * Node.M.super_seq_t
		| `Bibliography of Label.t * Order.preset_order_t
		| `Notes of Label.t * Order.preset_order_t
		| `Toc of Label.t * Order.preset_order_t
		] (*with sexp*)

	type top_block_t =
		[ `Heading of heading_block_t
		| `Title of Node.M.super_seq_t
		| `Subtitle of Node.M.super_seq_t
		| `Abstract of paragraph_block_t list
		| `Part of Node.M.super_seq_t
		| `Rule
		] (*with sexp*)

	type super_block_t = [ top_block_t | nestable_block_t ] (*with sexp*)

	type (+'a, 'b) t = 'a constraint 'a = [< super_block_t] (*with sexp*)

	let section level label order seq = `Heading (`Section (level, label, order, (seq :> Node.M.super_seq_t)))
	let appendix level label order seq = `Heading (`Appendix (level, label, order, (seq :> Node.M.super_seq_t)))
	let bibliography label order = `Heading (`Bibliography (label, order))
	let notes label order = `Heading (`Notes (label, order))
	let toc label order = `Heading (`Toc (label, order))

	let title seq = `Title (seq :> Node.M.super_seq_t)
	let subtitle seq = `Subtitle (seq :> Node.M.super_seq_t)
	let abstract frag = `Abstract frag
	let part seq = `Part (seq :> Node.M.super_seq_t)
	let rule () = `Rule

	let paragraph seq = `Paragraph (seq :> Node.M.super_seq_t)
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

