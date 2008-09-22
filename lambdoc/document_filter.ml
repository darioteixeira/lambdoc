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
let filter document_ast =

	let errors = DynArray.create () in

	let check feature target = Some target in

		(*
		let msg = Error.Invalid_composition_subset "section" in
		DynArray.add errors (comm.comm_linenum, msg);
		None
		*)


	(************************************************************************)
	(* Filters for inline context.						*)
	(************************************************************************)

	let rec filter_super_seq seq =
		ExtList.List.filter_map filter_super_node seq


	and filter_nonlink_seq seq =
		ExtList.List.filter_map filter_nonlink_node seq


	and filter_textual_seq seq =
		ExtList.List.filter_map filter_textual_node seq


	and filter_super_node = function
		| `AST_nonlink_node node ->
			(match filter_nonlink_node node with
				| Some node	-> Some (`AST_nonlink_node node)
				| None		-> None)
		| `AST_link_node node ->
			(match filter_link_node node with
				| Some node	-> Some (`AST_link_node node)
				| None		-> None)


	and filter_nonlink_node = function
		| `AST_textual node ->
			(match filter_textual_node node with
				| Some node	-> Some (`AST_textual node)
				| None		-> None)
		| `AST_mathtex_inl (op, txt) ->
			check `Feature_mathtex_inl (`AST_mathtex_inl (op, txt))
		| `AST_mathml_inl (op, txt) ->
			check `Feature_mathml_inl (`AST_mathml_inl (op, txt))
		| `AST_bold (comm, seq) ->
			check `Feature_bold (`AST_bold (comm, filter_super_seq seq))
		| `AST_emph (comm, seq) ->
			check `Feature_emph (`AST_emph (comm, filter_super_seq seq))
		| `AST_mono (comm, seq) ->
			check `Feature_mono (`AST_mono (comm, filter_super_seq seq))
		| `AST_caps (comm, seq) ->
			check `Feature_caps (`AST_caps (comm, filter_super_seq seq))
		| `AST_thru (comm, seq) ->
			check `Feature_thru (`AST_thru (comm, filter_super_seq seq))
		| `AST_sup (comm, seq) ->
			check `Feature_sup (`AST_sup (comm, filter_super_seq seq))
		| `AST_sub (comm, seq) ->
			check `Feature_sub (`AST_sub (comm, filter_super_seq seq))
		| `AST_box (comm, seq) ->
			check `Feature_box (`AST_box (comm, filter_super_seq seq))


	and filter_link_node = function
		| `AST_link (comm, lnk, seq) ->
			check `Feature_link (`AST_link (comm, lnk, filter_nonlink_seq seq))
		| `AST_see (comm, ref) ->
			check `Feature_see (`AST_see (comm, ref))
		| `AST_cite (comm, ref) ->
			check `Feature_cite (`AST_cite (comm, ref))
		| `AST_ref (comm, ref) ->
			check `Feature_ref (`AST_ref (comm, ref))
		| `AST_sref (comm, ref) ->
			check `Feature_sref (`AST_sref (comm, ref))
		| `AST_mref (comm, ref_, seq) ->
			check `Feature_mref (`AST_mref (comm, ref, seq))


	and filter_textual_node = function
		| `AST_plain txt ->
			check `Feature_plain (`AST_plain txt)
		| `AST_entity txt ->
			check `Feature_entity (`AST_entity txt) in


	(************************************************************************)
	(* Filter for tabular environment.					*)
	(************************************************************************)

	let filter_tabular tab = None in
		(*
		let filter_row (op, seqs) =
			(op, List.map filter_super_seq seqs) in
		let filter_group (comm, rows) =
			(comm, List.map filter_row rows) in
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
		*)


	(************************************************************************)
	(* Filters for document blocks.						*)
	(************************************************************************)

	let rec filter_super_frag frag =
		ExtList.List.filter_map filter_super_block frag


	and filter_nestable_frag frag =
		ExtList.List.filter_map filter_nestable_block frag


	and filter_super_block = function
		| `AST_top_block block ->
			(match filter_top_block block with
				| Some block	-> Some (`AST_top_block block)
				| None		-> None)
		| `AST_nestable_block block ->
			(match filter_nestable_block block with
				| Some block	-> Some (`AST_nestable_block block)
				| None		-> None)


	and filter_top_block = function
		| `AST_heading heading ->
			(match filter_heading heading with
				| Some heading	-> Some (`AST_heading heading)
				| None		-> None)
		| `AST_appendix comm ->
			check `Feature_appendix (`AST_appendix comm)
		| `AST_rule comm ->
			check `Feature_rule (`AST_rule comm)
		| `AST_setting (comm, key, value) ->
			check `Feature_setting (`AST_setting comm)


	and filter_heading = function
		| `AST_section (comm, seq) ->
			check `Feature_section (`AST_section (comm, filter_super_seq seq))
		| `AST_subsection (comm, seq) ->
			check `Feature_subsection (`AST_subsection (comm, filter_super_seq seq))
		| `AST_subsubsection (comm, seq) ->
			check `Feature_subsubsection (`AST_subsubsection (comm, filter_super_seq seq))
		| `AST_bibliography comm ->
			check `Feature_bibliography (`AST_bibliography comm)
		| `AST_notes comm ->
			check `Feature_notes (`AST_notes comm)
		| `AST_toc comm ->
			check `Feature_toc (`AST_toc comm)


	and filter_nestable_block = function
		| `AST_paragraph (op, seq) ->
			check `Feature_paragraph (`AST_paragraph (op, filter_super_seq seq))
		| `AST_itemize (comm, items) ->
			check `Feature_itemize (`AST_itemize (comm, filter_items items))
		| `AST_enumerate (comm, items) ->
			check `Feature_enumerate (`AST_enumerate (comm, filter_items items))
		| `AST_quote (comm, frag) ->
			check `Feature_quote (`AST_quote (comm, filter_nestable_frag frag))
		| `AST_mathtex_blk (comm, txt) ->
			check `Feature_mathtex_blk (`AST_mathtex_blk (comm, txt))
		| `AST_mathml_blk (comm, txt) ->
			check `Feature_mathml_blk (`AST_mathml_blk (comm, txt))
		| `AST_code (comm, seq) ->
			check `Feature_code (`AST_code (comm, seq))
		| `AST_verbatim (comm, seq) ->
			check `Feature_verbatim (`AST_verbatim (comm, seq))
		| `AST_tabular (comm, tab) ->
			check `Feature_tabular (`AST_tabular (comm, filter_tabular tab))
		| `AST_image (comm, alias) ->
			check `Feature_image (`AST_image (comm, alias))
		| `AST_subpage (comm, subpage) ->
			check `Feature_subpage (`AST_subpage (comm, filter_super_frag subpage))
		| `AST_equation (comm, caption, eq) ->
			check `Feature_equation (`AST_equation (comm, filter_super_seq caption, filter_equation eq))
		| `AST_algorithm (comm, caption, alg) ->
			check `Feature_algorithm (`AST_algorithm (comm, filter_super_seq caption, filter_algorithm alg))
		| `AST_table (comm, caption, tab) ->
			check `Feature_table (`AST_table (comm, filter_super_seq caption, filter_table tab))
		| `AST_figure (comm, caption, fig) ->
			check `Feature_figure (`AST_figure (comm, filter_super_seq caption, filter_figure fig))
		| `AST_bib (comm, title, author, resource) ->
			check `Feature_bib (`AST_bib (comm, filter_super_seq title, filter_super_seq author, filter_super_seq resource))
		| `AST_note (comm, seq) ->
			check `Feature_note (`AST_note (comm, seq))


	and filter_item = function
		| `AST_item (op, frag) ->
			`AST_Item (op, filter_nestable_frag frag)


	and filter_items items =
		List.map filter_item items


	and filter_equation = function
		| `AST_mathtex_blk (comm, txt) ->
			check `Feature_mathtex_blk (`AST_mathtex_blk (comm, txt))
		| `AST_mathml_blk (comm, txt) ->
			check `Feature_mathml_blk (`AST_mathml_blk (comm, txt))


	and filter_algorithm = function
		| `AST_code (comm, txt) ->
			check `Feature_code (`AST_code (comm, txt))


	and filter_table = function
		| `AST_tabular (comm, tab) ->
			(*
			check `Feature_tabular (`AST_tabular (comm, filter_tabular tab))
			*)
			None


	and filter_figure = function
		| `AST_image (comm, alias) ->
			check `Feature_image (`AST_image (comm, alias))
		| `AST_verbatim (comm, txt) ->
			check `Feature_verbatim (`AST_verbatim (comm, txt))
		| `AST_subpage (comm, subpage) ->
			check `Feature_subpage (`AST_subpage (comm, filter_super_frag subpage))


	in (filter_super_frag document_ast, DynArray.to_list errors)

