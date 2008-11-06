(********************************************************************************)
(*	Implementation file for Document_convert.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Conversion functions on documents.
*)

open Document_basic
open Document_node
open Document_block
open Document_tabular


(********************************************************************************)
(**	{2 Types and exceptions}						*)
(********************************************************************************)

exception Invalid_composition_subset of tag_t


(********************************************************************************)
(**	{2 Public functions}							*)
(********************************************************************************)

(**	Converts a document into a composition.
*)
let convert_to_composition contents =

	(************************************************************************)
	(* Conversion functions for inline context.				*)
	(************************************************************************)

	let rec convert_textual_node = function
		| `Plain txt			-> Node.plain txt
		| `Entity txt			-> Node.entity txt

	and convert_nonlink_node = function
		| #Node.textual_node_t as node	-> (convert_textual_node node :> ([> Node.nonlink_node_t], _) Node.t)
		| `Math math			-> Node.math math
		| `Bold seq			-> Node.bold (convert_super_seq seq)
		| `Emph seq			-> Node.emph (convert_super_seq seq)
		| `Mono seq			-> Node.mono (convert_super_seq seq)
		| `Caps seq			-> Node.caps (convert_super_seq seq)
		| `Thru seq			-> Node.thru (convert_super_seq seq)
		| `Sup seq			-> Node.sup (convert_super_seq seq)
		| `Sub seq			-> Node.sub (convert_super_seq seq)
		| `Box seq			-> Node.box (convert_super_seq seq)

	and convert_link_node = function
		| `Link (lnk, seq)		-> Node.link lnk (convert_nonlink_seq seq)
		| `See _			-> raise (Invalid_composition_subset "see")
		| `Cite _			-> raise (Invalid_composition_subset "cite")
		| `Ref _			-> raise (Invalid_composition_subset "ref")
		| `Sref _			-> raise (Invalid_composition_subset "sref")
		| `Mref _			-> raise (Invalid_composition_subset "mref")

        and convert_super_node = function
		| #Node.nonlink_node_t as node ->
			(convert_nonlink_node node :> (Node.super_node_t, _) Node.t)
		| #Node.link_node_t as node ->
			(convert_link_node node :> (Node.super_node_t, _) Node.t)

	and convert_nonlink_seq seq =
		List.map convert_nonlink_node seq

	and convert_super_seq seq =
		List.map convert_super_node seq in


	(************************************************************************)
	(* Conversion functions for tabular environment.			*)
	(************************************************************************)

	let convert_tabular tab =
		let convert_row (hd, tl) = Tabular.make_row (fplus convert_super_seq hd tl) in
		let convert_group (hd, tl) = fplus convert_row hd tl in
		let thead = match tab.Tabular.thead with
			| Some grp	-> Some (convert_group grp)
			| None		-> None
		and tfoot = match tab.Tabular.tfoot with
			| Some grp	-> Some (convert_group grp)
			| None		-> None
		and tbodies =
			let (hd, tl) = tab.Tabular.tbodies
			in fplus convert_group hd tl
		in Tabular.make tab.Tabular.tcols ?thead ?tfoot tbodies in


	(************************************************************************)
	(* Conversion functions for document blocks.				*)
	(************************************************************************)

	let rec convert_super_frag frag =
		List.map convert_super_block frag

	and convert_nestable_frag frag =
		List.map convert_nestable_block frag

	and convert_paragraph_block = function
		| `Paragraph seq -> Block.paragraph (convert_super_seq seq)

	and convert_itemize_block = function
		| `Itemize (bul, (hd, tl)) -> Block.itemize bul (fplus convert_nestable_frag hd tl)

	and convert_enumerate_block = function
		| `Enumerate (num, (hd, tl)) -> Block.enumerate num (fplus convert_nestable_frag hd tl)

	and convert_quote_block = function
		| `Quote (alignment, frag) -> Block.quote alignment (convert_nestable_frag frag)

	and convert_math_block = function
		| `Math (alignment, math) -> Block.math alignment math

	and convert_code_block = function
		| `Code (alignment, highlight) -> Block.code alignment highlight

	and convert_verbatim_block = function
		| `Verbatim (alignment, txt) -> Block.verbatim alignment txt

	and convert_tabular_block = function
		| `Tabular (alignment, tab) -> Block.tabular alignment (convert_tabular tab)

	and convert_image_block = function
		| `Image (alignment, alias) -> Block.image alignment alias

	and convert_subpage_block = function
		| `Subpage (alignment, frag) -> Block.subpage alignment (convert_super_frag frag)

	and convert_nestable_block = function
		| #Block.paragraph_block_t as blk ->
			(convert_paragraph_block blk :> (Block.nestable_block_t, _) Block.t)
		| #Block.itemize_block_t as blk ->
			(convert_itemize_block blk :> (Block.nestable_block_t, _) Block.t)
		| #Block.enumerate_block_t as blk ->
			(convert_enumerate_block blk :> (Block.nestable_block_t, _) Block.t)
		| #Block.quote_block_t as blk ->
			(convert_quote_block blk :> (Block.nestable_block_t, _) Block.t)
		| #Block.math_block_t as blk ->
			(convert_math_block blk :> (Block.nestable_block_t, _) Block.t)
		| #Block.code_block_t as blk ->
			(convert_code_block blk :> (Block.nestable_block_t, _) Block.t)
		| #Block.verbatim_block_t as blk ->
			(convert_verbatim_block blk :> (Block.nestable_block_t, _) Block.t)
		| #Block.tabular_block_t as blk ->
			(convert_tabular_block blk :> (Block.nestable_block_t, _) Block.t)
		| #Block.image_block_t as blk ->
			(convert_image_block blk :> (Block.nestable_block_t, _) Block.t)
		| #Block.subpage_block_t as blk ->
			(convert_subpage_block blk :> (Block.nestable_block_t, _) Block.t)
		| `Equation _ ->
			raise (Invalid_composition_subset "equation")
		| `Algorithm _ ->
			raise (Invalid_composition_subset "algorithm")
		| `Table _ ->
			raise (Invalid_composition_subset "table")
		| `Figure _ ->
			raise (Invalid_composition_subset "figure")

	and convert_heading = function
		| `Section _ ->
			raise (Invalid_composition_subset "section")
		| `Subsection _ ->
			raise (Invalid_composition_subset "subsection")
		| `Subsubsection _ ->
			raise (Invalid_composition_subset "subsubsection")
		| `Appendix _ ->
			raise (Invalid_composition_subset "appendix")
		| `Subappendix _ ->
			raise (Invalid_composition_subset "subappendix")
		| `Subsubappendix _ ->
			raise (Invalid_composition_subset "subsubappendix")
		| `Bibliography _ ->
			raise (Invalid_composition_subset "bibliography")
		| `Notes _ ->
			raise (Invalid_composition_subset "notes")
		| `Toc _ ->
			raise (Invalid_composition_subset "toc")

	and convert_top_block = function
		| `Title _ ->
			raise (Invalid_composition_subset "title")
		| `Subtitle _ ->
			raise (Invalid_composition_subset "subtitle")
		| `Abstract _ ->
			raise (Invalid_composition_subset "abstract")
		| `Rule ->
			raise (Invalid_composition_subset "rule")
		| `Heading heading ->
			convert_heading heading

	and convert_super_block = function
		| #Block.top_block_t as blk ->
			(convert_top_block blk :> (Block.super_block_t, _) Block.t)
		| #Block.nestable_block_t as blk ->
			(convert_nestable_block blk :> (Block.super_block_t, _) Block.t)

	in convert_super_frag contents

