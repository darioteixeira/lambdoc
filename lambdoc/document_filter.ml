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
		| Textual node ->
			Some (Textual node)
		| Mathtex (op, txt) ->
			Some (Mathtex (op, txt))
		| Mathml (op, txt) ->
			Some (Mathml (op, txt))
		| Bold (params, seq) ->
			Some (Bold (params, filter_super_seq seq))
		| Emph (params, seq) ->
			Some (Emph (params, filter_super_seq seq))
		| Mono (params, seq) ->
			Some (Mono (params, filter_super_seq seq))
		| Caps (params, seq) ->
			Some (Caps (params, filter_super_seq seq))
		| Thru (params, seq) ->
			Some (Thru (params, filter_super_seq seq))
		| Sup (params, seq) ->
			Some (Sup (params, filter_super_seq seq))
		| Sub (params, seq) ->
			Some (Sub (params, filter_super_seq seq))
		| Box (params, seq) ->
			Some (Box (params, filter_super_seq seq))

	and filter_link_node = function
		| Link (params, lnk, seq) ->
			Some (Link (params, lnk, filter_nonlink_seq seq))
		| See (params, _) ->
			let msg = Error.Invalid_composition_subset "see" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| Cite (params, _) ->
			let msg = Error.Invalid_composition_subset "cite" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| Ref (params, _) ->
			let msg = Error.Invalid_composition_subset "ref" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| Sref (params, _) ->
			let msg = Error.Invalid_composition_subset "sref" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| Mref (params, _, _) ->
			let msg = Error.Invalid_composition_subset "mref" in
			DynArray.add errors (params.comm_linenum, msg);
			None

	and filter_super_node = function
		| Nonlink_node node ->
			(match filter_nonlink_node node with
				| Some node	-> Some (Nonlink_node node)
				| None		-> None)
		| Link_node node ->
			(match filter_link_node node with
				| Some node	-> Some (Link_node node)
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
		| Section (params, _) ->
			let msg = Error.Invalid_composition_subset "section" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| Subsection (params, _) ->
			let msg = Error.Invalid_composition_subset "subsection" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| Subsubsection (params, _) ->
			let msg = Error.Invalid_composition_subset "subsubsection" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| Bibliography params ->
			let msg = Error.Invalid_composition_subset "bibliography" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| Notes params ->
			let msg = Error.Invalid_composition_subset "notes" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| Toc params ->
			let msg = Error.Invalid_composition_subset "toc" in
			DynArray.add errors (params.comm_linenum, msg);
			None

	and filter_top_block = function
		| Heading heading ->
			(match filter_heading heading with
				| Some heading	-> Some (Heading heading)
				| None		-> None)
		| Appendix params ->
			let msg = Error.Invalid_composition_subset "appendix" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| Rule params ->
			let msg = Error.Invalid_composition_subset "rule" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| Setting (params, key, value) ->
			let msg = Error.Invalid_composition_subset "set" in
			DynArray.add errors (params.comm_linenum, msg);
			None

	and filter_nestable_block = function
		| Paragraph (op, seq) ->
			Some (Paragraph (op, filter_super_seq seq))
		| Math (params, txt) ->
			Some (Math (params, txt))
		| Tabular (params, tab) ->
			Some (Tabular (params, filter_tabular tab))
		| Preformat (params, txt) ->
			Some (Preformat (params, txt))
		| Itemize (params, items) ->
			Some (Itemize (params, filter_items items))
		| Enumerate (params, items) ->
			Some (Enumerate (params, filter_items items))
		| Quote (params, frag) ->
			Some (Quote (params, filter_nestable_frag frag))
		| Algorithm (params, _) ->
			let msg = Error.Invalid_composition_subset "algorithm" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| Equation (params, _) ->
			let msg = Error.Invalid_composition_subset "equation" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| Figure (params, _) ->
			let msg = Error.Invalid_composition_subset "figure" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| Table (params, _) ->
			let msg = Error.Invalid_composition_subset "table" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| Bib (params, _) ->
			let msg = Error.Invalid_composition_subset "bib" in
			DynArray.add errors (params.comm_linenum, msg);
			None
		| Note (params, _) ->
			let msg = Error.Invalid_composition_subset "note" in
			DynArray.add errors (params.comm_linenum, msg);
			None

	and filter_item = function
		| Item (op, frag) ->
			Item (op, filter_nestable_frag frag)

	and filter_items items =
		List.map filter_item items

	and filter_super_block = function
		| Top_block block ->
			(match filter_top_block block with
				| Some block	-> Some (Top_block block)
				| None		-> None)
		| Nestable_block block ->
			(match filter_nestable_block block with
				| Some block	-> Some (Nestable_block block)
				| None		-> None)

	and filter_nestable_frag frag =
		ExtList.List.filter_map filter_nestable_block frag

	and filter_super_frag frag =
		ExtList.List.filter_map filter_super_block frag

	in (filter_super_frag document_ast, DynArray.to_list errors)

