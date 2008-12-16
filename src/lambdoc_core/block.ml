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
	(**	Definition of the ordering types for the various kinds of blocks.
	*)

	type part_order_t = (Order.ordinal_t as 'a, [ 'a Order.auto_given_t | Order.user_given_t | Order.none_given_t ]) Order.t (*with sexp*)
	type section_order_t = (Order.hierarchical_t as 'a, [ 'a Order.auto_given_t | Order.user_given_t | Order.none_given_t ]) Order.t (*with sexp*)
	type wrapper_order_t = (Order.ordinal_t as 'a, [ 'a Order.auto_given_t | Order.user_given_t ]) Order.t (*with sexp*)
	type bib_order_t = (Order.ordinal_t as 'a, 'a Order.auto_given_t) Order.t (*with sexp*)
	type note_order_t = (Order.ordinal_t as 'a, 'a Order.auto_given_t) Order.t (*with sexp*)


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
	type quote_block_t = [ `Quote of Floater.t * nestable_frag_t ] (*with sexp*)
	type math_block_t = [ `Math of Floater.t * Math.t ] (*with sexp*) 
	type code_block_t = [ `Code of Floater.t * Code.t ] (*with sexp*) 
	type tabular_block_t = [ `Tabular of Floater.t * Tabular.t ] (*with sexp*) 
	type image_block_t = [ `Image of Floater.t * alias_t ] (*with sexp*) 
	type verbatim_block_t = [ `Verbatim of Floater.t * raw_t ] (*with sexp*) 
	type subpage_block_t = [ `Subpage of Floater.t * super_frag_t ] (*with sexp*) 


	(**	The various types of wrapper blocks.  Wrappers are just
		numbered containers around some kinds of nestable blocks.
	*)

	type equation_block_t = [ math_block_t ] (*with sexp*)
	type printout_block_t = [ code_block_t ] (*with sexp*)
	type table_block_t = [ tabular_block_t ] (*with sexp*)
	type figure_block_t = [ image_block_t | verbatim_block_t | subpage_block_t ] (*with sexp*)


	(**	The tuple of all common fields to wrappers.  The fields
		are the wrapper's label, its order, and a caption.
	*)
	type wrapper_t = Label.t * wrapper_order_t * Node.M.super_seq_t (*with sexp*)


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
		| `Printout of wrapper_t * printout_block_t
		| `Table of wrapper_t * table_block_t
		| `Figure of wrapper_t * figure_block_t
		] (*with sexp*)


	(**	Part content.
	*)
	type part_content_t =
		[ `Custom of Node.M.super_seq_t
		| `Appendix
		] (*with sexp*)


	(**	Section content.
	*)
	type section_content_t =
		[ `Custom of Node.M.super_seq_t
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


	(**	Top blocks are those that may not be nested, and therefore appear only
		at the highest level in the document structure.
	*)
	type top_block_t =
		[ heading_block_t
		| `Title of title_level_t * Node.M.super_seq_t
		| `Abstract of paragraph_block_t list
		| `Rule
		] (*with sexp*)


	(**	A super block is either a top block or a nestable block.
	*)
	type super_block_t = [ top_block_t | nestable_block_t ] (*with sexp*)

	type (+'a, 'b) t = 'a constraint 'a = [< super_block_t] (*with sexp*)

	val part: Label.t -> part_order_t -> ([< Node.M.super_node_t ], 'b) Node.M.t list ->
		([> heading_block_t ], [> `Manuscript ]) t
	val section: Label.t -> section_order_t -> section_location_t -> hierarchical_level_t -> ([< Node.M.super_node_t ], 'b) Node.M.t list ->
		([> heading_block_t ], [> `Manuscript ]) t
	val appendix: Label.t ->
		([> heading_block_t ], [> `Manuscript ]) t
	val bibliography: Label.t ->
		([> heading_block_t ], [> `Manuscript ]) t
	val notes: Label.t -> 
		([> heading_block_t ], [> `Manuscript ]) t
	val toc: Label.t -> 
		([> heading_block_t ], [> `Manuscript ]) t
	val title: title_level_t -> ([< Node.M.super_node_t ], 'b) Node.M.t list ->
		([> top_block_t ], [> `Manuscript ]) t
	val abstract: (paragraph_block_t, 'b) t list ->
		([> top_block_t ], [> `Manuscript ]) t
	val rule: unit ->
		([> top_block_t ], [> `Manuscript ]) t

	val paragraph: ([< Node.M.super_node_t ], 'b) Node.M.t list ->
		([> paragraph_block_t ], 'b) t
	val itemize: Bullet.t -> (nestable_block_t, 'b) t list plus_t ->
		([> itemize_block_t ], 'b) t
	val enumerate: Numbering.t -> (nestable_block_t, 'b) t list plus_t ->
		([> enumerate_block_t ], 'b) t
	val quote: Floater.t -> (nestable_block_t, 'b) t list ->
		([> quote_block_t ], 'b) t
	val math: Floater.t -> Math.t ->
		([> math_block_t ], [> `Composition]) t
	val code: Floater.t -> Code.t ->
		([> code_block_t ], [> `Composition]) t
	val verbatim: Floater.t -> raw_t ->
		([> verbatim_block_t ], [> `Composition]) t
	val tabular: Floater.t -> Tabular.t ->
		([> tabular_block_t ], [> `Manuscript]) t
	val image: Floater.t -> alias_t ->
		([> image_block_t ], [> `Composition]) t
	val subpage: Floater.t -> ([< super_block_t ], 'b) t list ->
		([> subpage_block_t ], 'b) t

	val equation: wrapper_t -> equation_block_t ->
		([> nestable_block_t ], [> `Manuscript]) t
	val printout: wrapper_t -> printout_block_t ->
		([> nestable_block_t ], [> `Manuscript]) t
	val table: wrapper_t -> table_block_t ->
		([> nestable_block_t ], [> `Manuscript]) t
	val figure: wrapper_t -> figure_block_t ->
		([> nestable_block_t ], [> `Manuscript]) t
end =
struct
	type part_order_t = (Order.ordinal_t as 'a, [ 'a Order.auto_given_t | Order.user_given_t | Order.none_given_t ]) Order.t (*with sexp*)
	type section_order_t = (Order.hierarchical_t as 'a, [ 'a Order.auto_given_t | Order.user_given_t | Order.none_given_t ]) Order.t (*with sexp*)
	type wrapper_order_t = (Order.ordinal_t as 'a, [ 'a Order.auto_given_t | Order.user_given_t ]) Order.t (*with sexp*)
	type bib_order_t = (Order.ordinal_t as 'a, 'a Order.auto_given_t) Order.t (*with sexp*)
	type note_order_t = (Order.ordinal_t as 'a, 'a Order.auto_given_t) Order.t (*with sexp*)

	type super_frag_t = M.super_block_t list (*with sexp*)

	type nestable_frag_t = M.nestable_block_t list (*with sexp*)

	type paragraph_block_t = [ `Paragraph of Node.M.super_seq_t ] (*with sexp*)
	type itemize_block_t = [ `Itemize of Bullet.t * nestable_frag_t plus_t ] (*with sexp*)
	type enumerate_block_t = [ `Enumerate of Numbering.t * nestable_frag_t plus_t ] (*with sexp*)
	type quote_block_t = [ `Quote of Floater.t * nestable_frag_t ] (*with sexp*)
	type math_block_t = [ `Math of Floater.t * Math.t ] (*with sexp*) 
	type code_block_t = [ `Code of Floater.t * Code.t ] (*with sexp*) 
	type tabular_block_t = [ `Tabular of Floater.t * Tabular.t ] (*with sexp*) 
	type image_block_t = [ `Image of Floater.t * alias_t ] (*with sexp*) 
	type verbatim_block_t = [ `Verbatim of Floater.t * raw_t ] (*with sexp*) 
	type subpage_block_t = [ `Subpage of Floater.t * super_frag_t ] (*with sexp*) 

	type equation_block_t = [ math_block_t ] (*with sexp*)
	type printout_block_t = [ code_block_t ] (*with sexp*)
	type table_block_t = [ tabular_block_t ] (*with sexp*)
	type figure_block_t = [ image_block_t | verbatim_block_t | subpage_block_t ] (*with sexp*)

	type wrapper_t = Label.t * wrapper_order_t * Node.M.super_seq_t (*with sexp*)

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
		| `Printout of wrapper_t * printout_block_t
		| `Table of wrapper_t * table_block_t
		| `Figure of wrapper_t * figure_block_t
		] (*with sexp*)

	type part_content_t =
		[ `Custom of Node.M.super_seq_t
		| `Appendix
		] (*with sexp*)

	type section_content_t =
		[ `Custom of Node.M.super_seq_t
		| `Bibliography
		| `Notes
		| `Toc
		] (*with sexp*)

	type section_location_t =
		[ `Mainbody
		| `Appendixed
		] (*with sexp*)

	type heading_block_t =
		[ `Part of Label.t * part_order_t * part_content_t
		| `Section of Label.t * section_order_t * section_location_t * hierarchical_level_t * section_content_t
		] (*with sexp*)

	type top_block_t =
		[ heading_block_t
		| `Title of title_level_t * Node.M.super_seq_t
		| `Abstract of paragraph_block_t list
		| `Rule
		] (*with sexp*)

	type super_block_t = [ top_block_t | nestable_block_t ] (*with sexp*)

	type (+'a, 'b) t = 'a constraint 'a = [< super_block_t] (*with sexp*)

	let part label order seq = `Part (label, order, `Custom (seq :> Node.M.super_seq_t))
	let section label order location level seq = `Section (label, order, location, level, `Custom (seq :> Node.M.super_seq_t))
	let appendix label = `Part (label, Order.none (), `Appendix)
	let bibliography label = `Section (label, Order.none (), `Mainbody, `Level1, `Bibliography)
	let notes label = `Section (label, Order.none (), `Mainbody, `Level1, `Notes)
	let toc label = `Section (label, Order.none (), `Mainbody, `Level1, `Toc)
	let title level seq = `Title (level, (seq :> Node.M.super_seq_t))
	let abstract frag = `Abstract frag
	let rule () = `Rule

	let paragraph seq = `Paragraph (seq :> Node.M.super_seq_t)
	let itemize bullet (head_frag, tail_frags) = `Itemize (bullet, (head_frag, tail_frags))
	let enumerate numbering (head_frag, tail_frags) = `Enumerate (numbering, (head_frag, tail_frags))
	let quote floater frag = `Quote (floater, frag)
	let math floater math = `Math (floater, math)
	let code floater code = `Code (floater, code)
	let verbatim floater txt = `Verbatim (floater, txt)
	let tabular floater tab = `Tabular (floater, tab)
	let image floater alias = `Image (floater, alias)
	let subpage floater frag = `Subpage (floater, (frag :> super_block_t list))

	let equation (label, order, caption) math = `Equation ((label, order, caption), math)
	let printout (label, order, caption) code = `Printout ((label, order, caption), code)
	let table (label, order, caption) tabular = `Table ((label, order, caption), tabular)
	let figure (label, order, caption) figure = `Figure ((label, order, caption), figure)
end

