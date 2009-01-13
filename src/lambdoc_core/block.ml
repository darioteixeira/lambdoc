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

(*TYPE_CONV_PATH "Document"*)

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


	(**	Common definitions for image types (bitmap and vectorial pictures).
	*)
	type image_t = bool * bool * int option * alias_t * XHTML.M.text


	(**	The various types of individual building blocks.
	*)

	type paragraph_block_t = [ `Paragraph of Node.M.super_seq_t ] (*with sexp*)
	type itemize_block_t = [ `Itemize of Bullet.t * nestable_frag_t plus_t ] (*with sexp*)
	type enumerate_block_t = [ `Enumerate of Numbering.t * nestable_frag_t plus_t ] (*with sexp*)
	type quote_block_t = [ `Quote of Alignment.t * nestable_frag_t ] (*with sexp*)
	type math_block_t = [ `Math of Alignment.t * Math.t ] (*with sexp*) 
	type code_block_t = [ `Code of Alignment.t * bool * bool * Code.t ] (*with sexp*) 
	type tabular_block_t = [ `Tabular of Alignment.t * Tabular.t ] (*with sexp*) 
	type bitmap_block_t = [ `Bitmap of Alignment.t * image_t ] (*with sexp*) 
	type verbatim_block_t = [ `Verbatim of Alignment.t * raw_t ] (*with sexp*) 
	type subpage_block_t = [ `Subpage of Alignment.t * super_frag_t ] (*with sexp*)


	(**	The various types of wrapper blocks.  Wrappers are just
		numbered containers around some kinds of basic blocks.
	*)

	type equation_block_t = math_block_t (*with sexp*)
	type printout_block_t = code_block_t (*with sexp*)
	type table_block_t = tabular_block_t (*with sexp*)
	type figure_block_t = [ bitmap_block_t | verbatim_block_t | subpage_block_t ] (*with sexp*)


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
		| bitmap_block_t
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

	type (+'a, 'b) t = 'a constraint 'a = [< super_block_t ] (*with sexp*)

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
	val quote: Alignment.t -> (nestable_block_t, 'b) t list ->
		([> quote_block_t ], 'b) t
	val math: Alignment.t -> Math.t ->
		([> math_block_t ], [> `Composition]) t
	val code: Alignment.t -> bool -> bool -> Code.t ->
		([> code_block_t ], [> `Composition]) t
	val verbatim: Alignment.t -> raw_t ->
		([> verbatim_block_t ], [> `Composition]) t
	val tabular: Alignment.t -> Tabular.t ->
		([> tabular_block_t ], [> `Manuscript]) t
	val bitmap: Alignment.t -> image_t ->
		([> bitmap_block_t ], 'b) t
	val subpage: Alignment.t -> ([< super_block_t ], 'b) t list ->
		([> subpage_block_t ], 'b) t
	val equation: wrapper_t -> (equation_block_t, _) t ->
		([> nestable_block_t ], [> `Manuscript]) t
	val printout: wrapper_t -> (printout_block_t, _) t ->
		([> nestable_block_t ], [> `Manuscript]) t
	val table: wrapper_t -> (table_block_t, _) t ->
		([> nestable_block_t ], [> `Manuscript]) t
	val figure: wrapper_t -> (figure_block_t, _) t ->
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

	type image_t = bool * bool * int option * alias_t * XHTML.M.text

	type paragraph_block_t = [ `Paragraph of Node.M.super_seq_t ] (*with sexp*)
	type itemize_block_t = [ `Itemize of Bullet.t * nestable_frag_t plus_t ] (*with sexp*)
	type enumerate_block_t = [ `Enumerate of Numbering.t * nestable_frag_t plus_t ] (*with sexp*)
	type quote_block_t = [ `Quote of Alignment.t * nestable_frag_t ] (*with sexp*)
	type math_block_t = [ `Math of Alignment.t * Math.t ] (*with sexp*) 
	type code_block_t = [ `Code of Alignment.t * bool * bool * Code.t ] (*with sexp*) 
	type tabular_block_t = [ `Tabular of Alignment.t * Tabular.t ] (*with sexp*) 
	type bitmap_block_t = [ `Bitmap of Alignment.t * image_t ] (*with sexp*) 
	type verbatim_block_t = [ `Verbatim of Alignment.t * raw_t ] (*with sexp*) 
	type subpage_block_t = [ `Subpage of Alignment.t * super_frag_t ] (*with sexp*)

	type equation_block_t = math_block_t (*with sexp*)
	type printout_block_t = code_block_t (*with sexp*)
	type table_block_t = tabular_block_t (*with sexp*)
	type figure_block_t = [ bitmap_block_t | verbatim_block_t | subpage_block_t ] (*with sexp*)

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
		| bitmap_block_t
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

	type (+'a, 'b) t = 'a constraint 'a = [< super_block_t ] (*with sexp*)

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
	let quote alignment frag = `Quote (alignment, frag)
	let math alignment mth = `Math (alignment, mth)
	let code alignment linenums zebra txt = `Code (alignment, linenums, zebra, txt)
	let verbatim alignment txt = `Verbatim (alignment, txt)
	let tabular alignment tab = `Tabular (alignment, tab)
	let bitmap alignment image = `Bitmap (alignment, image)
	let subpage alignment frag = `Subpage (alignment, (frag :> super_block_t list))
	let equation wrapper equation_blk = `Equation (wrapper, equation_blk)
	let printout wrapper printout_blk = `Printout (wrapper, printout_blk)
	let table wrapper table_blk = `Table (wrapper, table_blk)
	let figure wrapper figure_blk = `Figure (wrapper, figure_blk)
end

