(********************************************************************************)
(**	Filtering functions on documents.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open ExtString
open Document_ast
open Document_error
open Document_tabular


(********************************************************************************)
(**	{2 Public functions}							*)
(********************************************************************************)

(**	Given an AST from the parser, this function filters out all nodes that
	are not allowed for compositions.  Besides the new, filtered AST, this
	function returns also a list with all the invalid nodes found.
*)
let filter_to_composition document_ast =

	let errors = DynArray.create () in

	let rec filter_nonlink_node = function
		| `AST_textual node ->
			Some (`AST_textual node)
		| `AST_mathtex_inl (op, txt) ->
			Some (`AST_mathtex_inl (op, txt))
		| `AST_mathml_inl (op, txt) ->
			Some (`AST_mathml_inl (op, txt))
		| `AST_bold (params, seq) ->
			Some (`AST_bold (params, filter_super_seq seq))
		| `AST_emph (params, seq) ->
			Some (`AST_emph (params, filter_super_seq seq))
		| `AST_mono (params, seq) ->
			Some (`AST_mono (params, filter_super_seq seq))
		| `AST_caps (params, seq) ->
			Some (`AST_caps (params, filter_super_seq seq))
		| `AST_thru (params, seq) ->
			Some (`AST_thru (params, filter_super_seq seq))
		| `AST_sup (params, seq) ->
			Some (`AST_sup (params, filter_super_seq seq))
		| `AST_sub (params, seq) ->
			Some (`AST_sub (params, filter_super_seq seq))
		| `AST_box (params, seq) ->
			Some (`AST_box (params, filter_super_seq seq))

	and filter_link_node = function
		| `AST_link (params, lnk, seq) ->
			Some (`AST_link (params, lnk, filter_nonlink_seq seq))
		| `AST_see (params, _) ->
			let msg = Error.Invalid_composition_subset "see" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| `AST_cite (params, _) ->
			let msg = Error.Invalid_composition_subset "cite" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| `AST_ref (params, _) ->
			let msg = Error.Invalid_composition_subset "ref" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| `AST_sref (params, _) ->
			let msg = Error.Invalid_composition_subset "sref" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| `AST_mref (params, _, _) ->
			let msg = Error.Invalid_composition_subset "mref" in
			DynArray.add errors (params.comm_linenum, msg);
			None

	and filter_super_node = function
		| `AST_nonlink_node node ->
			(match filter_nonlink_node node with
				| Some node	-> Some (`AST_nonlink_node node)
				| None		-> None)
		| `AST_link_node node ->
			(match filter_link_node node with
				| Some node	-> Some (`AST_link_node node)
				| None		-> None)

	and filter_super_seq seq =
		ExtList.List.filter_map filter_super_node seq

	and filter_nonlink_seq seq =
		ExtList.List.filter_map filter_nonlink_node seq

	and filter_tabular tab =
		let filter_row (op, seqs) =
			(op, List.map filter_super_seq seqs) in
		let filter_group (params, rows) =
			(params, List.map filter_row rows) in
		let thead = match tab.thead with
			| Some grp	-> Some (filter_group grp)
			| None		-> None
		and tfoot = match tab.tfoot with
			| Some grp	-> Some (filter_group grp)
			| None		-> None
		and tbodies =
			List.map filter_group tab.tbodies
		in	{
			thead = thead;
			tfoot = tfoot;
			tbodies = tbodies;
			}

	and filter_heading = function
		| `AST_section (params, _) ->
			let msg = Error.Invalid_composition_subset "section" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| `AST_subsection (params, _) ->
			let msg = Error.Invalid_composition_subset "subsection" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| `AST_subsubsection (params, _) ->
			let msg = Error.Invalid_composition_subset "subsubsection" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| `AST_bibliography params ->
			let msg = Error.Invalid_composition_subset "bibliography" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| `AST_notes params ->
			let msg = Error.Invalid_composition_subset "notes" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| `AST_toc params ->
			let msg = Error.Invalid_composition_subset "toc" in
			DynArray.add errors (params.comm_linenum, msg);
			None

	and filter_top_block = function
		| `AST_heading heading ->
			(match filter_heading heading with
				| Some heading	-> Some (`AST_heading heading)
				| None		-> None)
		| `AST_appendix params ->
			let msg = Error.Invalid_composition_subset "appendix" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| `AST_rule params ->
			let msg = Error.Invalid_composition_subset "rule" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| `AST_setting (params, key, value) ->
			let msg = Error.Invalid_composition_subset "set" in
			DynArray.add errors (params.comm_linenum, msg);
			None

	and filter_nestable_block = function
		| `AST_paragraph (op, seq) ->
			Some (`AST_paragraph (op, filter_super_seq seq))
		| `AST_itemize (params, items) ->
			Some (`AST_itemize (params, filter_items items))
		| `AST_enumerate (params, items) ->
			Some (`AST_enumerate (params, filter_items items))
		| `AST_quote (params, frag) ->
			Some (`AST_quote (params, filter_nestable_frag frag))
		| `AST_mathtex_blk (params, txt) ->
			Some (`AST_mathtex_blk (params, txt))
		| `AST_mathml_blk (params, txt) ->
			Some (`AST_mathml_blk (params, txt))
		| `AST_code (params, txt) ->
			Some (`AST_code (params, txt))
		| `AST_verbatim (params, txt) ->
			Some (`AST_verbatim (params, txt))
		| `AST_tabular (params, tab) ->
			Some (`AST_tabular (params, filter_tabular tab))
		| `AST_image (params, alias) ->
			Some (`AST_image (params, alias))
		| `AST_subpage (params, subpage) ->
			Some (`AST_subpage (params, filter_super_frag subpage))
		| `AST_equation (params, _) ->
			let msg = Error.Invalid_composition_subset "equation" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| `AST_algorithm (params, _) ->
			let msg = Error.Invalid_composition_subset "algorithm" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| `AST_table (params, _) ->
			let msg = Error.Invalid_composition_subset "table" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| `AST_figure (params, _) ->
			let msg = Error.Invalid_composition_subset "figure" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| `AST_bib (params, _) ->
			let msg = Error.Invalid_composition_subset "bib" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| `AST_note (params, _) ->
			let msg = Error.Invalid_composition_subset "note" in
			DynArray.add errors (params.comm_linenum, msg);
			None

	and filter_item = function
		| `AST_item (op, frag) ->
			`AST_Item (op, filter_nestable_frag frag)

	and filter_items items =
		List.map filter_item items

	and filter_super_block = function
		| `AST_top_block block ->
			(match filter_top_block block with
				| Some block	-> Some (`AST_top_block block)
				| None		-> None)
		| `AST_nestable_block block ->
			(match filter_nestable_block block with
				| Some block	-> Some (`AST_nestable_block block)
				| None		-> None)

	and filter_nestable_frag frag =
		ExtList.List.filter_map filter_nestable_block frag

	and filter_super_frag frag =
		ExtList.List.filter_map filter_super_block frag

	in (filter_super_frag document_ast, DynArray.to_list errors)

