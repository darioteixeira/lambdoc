(********************************************************************************)
(*	Implementation file for Convert module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Conversion functions on documents.
*)

open Lambdoc_core
open Basic


(********************************************************************************)
(**	{2 Exceptions}								*)
(********************************************************************************)

exception Invalid_composition_subset of tag_t


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

(**	Makes sure a generic document only uses features valid for compositions.
*)
let convert_to_composition contents =

	(************************************************************************)
	(* Conversion functions for inline context.				*)
	(************************************************************************)

	let rec convert_super_seq seq =
		List.map convert_super_node seq

	and convert_nonlink_seq seq =
		List.map convert_nonlink_node seq

        and convert_super_node = function
		| #Node.M.nonlink_node_t as node -> (convert_nonlink_node node :> (Node.M.super_node_t, _) Node.M.t)
		| #Node.M.link_node_t as node	 -> (convert_link_node node :> (Node.M.super_node_t, _) Node.M.t)

	and convert_nonlink_node = function
		| `Plain txt			-> Node.M.plain txt
		| `Entity txt			-> Node.M.entity txt
		| `Math math			-> Node.M.math math
		| `Bold seq			-> Node.M.bold (convert_super_seq seq)
		| `Emph seq			-> Node.M.emph (convert_super_seq seq)
		| `Mono seq			-> Node.M.mono (convert_super_seq seq)
		| `Caps seq			-> Node.M.caps (convert_super_seq seq)
		| `Thru seq			-> Node.M.thru (convert_super_seq seq)
		| `Sup seq			-> Node.M.sup (convert_super_seq seq)
		| `Sub seq			-> Node.M.sub (convert_super_seq seq)
		| `Mbox seq			-> Node.M.mbox (convert_super_seq seq)

	and convert_link_node = function
		| `Link (lnk, seq)		-> Node.M.link lnk (convert_nonlink_seq seq)
		| `See _			-> raise (Invalid_composition_subset "see")
		| `Cite _			-> raise (Invalid_composition_subset "cite")
		| `Ref _			-> raise (Invalid_composition_subset "ref")
		| `Sref _			-> raise (Invalid_composition_subset "sref")
		| `Mref _			-> raise (Invalid_composition_subset "mref") in


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

	and convert_super_block = function
		| #Block.M.top_block_t as blk		-> (convert_top_block blk :> (Block.M.super_block_t, _) Block.M.t)
		| #Block.M.nestable_block_t as blk	-> (convert_nestable_block blk :> (Block.M.super_block_t, _) Block.M.t)

	and convert_top_block = function
		| #Block.M.heading_block_t as blk	-> (convert_heading_block blk :> (Block.M.top_block_t, _) Block.M.t)
		| `Title _				-> raise (Invalid_composition_subset "title")
		| `Abstract _				-> raise (Invalid_composition_subset "abstract")
		| `Rule					-> raise (Invalid_composition_subset "rule")

	and convert_heading_block = function
		| `Part _				-> raise (Invalid_composition_subset "part")
		| `Section _				-> raise (Invalid_composition_subset "section")
		| `Appendix _				-> raise (Invalid_composition_subset "appendix")
		| `Bibliography _			-> raise (Invalid_composition_subset "bibliography")
		| `Notes _				-> raise (Invalid_composition_subset "notes")
		| `Toc _				-> raise (Invalid_composition_subset "toc")

	and convert_nestable_block = function
		| #Block.M.paragraph_block_t as blk	-> convert_paragraph_block blk
		| #Block.M.itemize_block_t as blk	-> convert_itemize_block blk
		| #Block.M.enumerate_block_t as blk	-> convert_enumerate_block blk
		| #Block.M.quote_block_t as blk		-> convert_quote_block blk
		| #Block.M.math_block_t as blk		-> convert_math_block blk
		| #Block.M.code_block_t as blk		-> convert_code_block blk
		| #Block.M.verbatim_block_t as blk	-> convert_verbatim_block blk
		| #Block.M.tabular_block_t as blk	-> convert_tabular_block blk
		| #Block.M.bitmap_block_t as blk	-> convert_bitmap_block blk
		| #Block.M.subpage_block_t as blk	-> convert_subpage_block blk
		| `Equation _				-> raise (Invalid_composition_subset "equation")
		| `Printout _				-> raise (Invalid_composition_subset "printout")
		| `Table _				-> raise (Invalid_composition_subset "table")
		| `Figure _				-> raise (Invalid_composition_subset "figure")

	and convert_paragraph_block (`Paragraph seq)			= Block.M.paragraph (convert_super_seq seq)
	and convert_itemize_block (`Itemize (bul, (hd, tl)))		= Block.M.itemize bul (fplus convert_nestable_frag hd tl)
	and convert_enumerate_block (`Enumerate (num, (hd, tl)))	= Block.M.enumerate num (fplus convert_nestable_frag hd tl)
	and convert_quote_block (`Quote (alignment, frag))		= Block.M.quote alignment (convert_nestable_frag frag)
	and convert_math_block (`Math (alignment, math))		= Block.M.math alignment math
	and convert_code_block (`Code (alignment, linenums, code))	= Block.M.code alignment linenums code
	and convert_verbatim_block (`Verbatim (alignment, txt))		= Block.M.verbatim alignment txt
	and convert_tabular_block (`Tabular (alignment, tab))		= Block.M.tabular alignment (convert_tabular tab)
	and convert_bitmap_block (`Bitmap (alignment, alias))		= Block.M.bitmap alignment alias
	and convert_subpage_block (`Subpage (alignment, frag))		= Block.M.subpage alignment (convert_super_frag frag)

	in convert_super_frag contents

