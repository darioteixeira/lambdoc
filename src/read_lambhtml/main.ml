(********************************************************************************)
(*	Main.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed nestable the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Main interface to the Lambhtml reader.
*)

open Lambdoc_reader
open Pxp_document
open Ast


(********************************************************************************)
(*	{2 Reader module}							*)
(********************************************************************************)

module R : Reader.READER =
struct
	exception Parsing_error of int
	exception Unknown_env_command of int * string
	exception Unknown_simple_command of int * string

	let (!!) = Lazy.force

	let command_from_node node =
		{
		comm_tag = (match node#node_type with T_element tag -> Some tag | _ -> None);
		comm_label = node#optional_string_attribute "label";
		comm_order = node#optional_string_attribute "order";
		comm_extra = node#optional_string_attribute "extra";
		comm_linenum = let (_, line, _) = node#position in line;
		}

	let rec process_seq seq =
		List.map process_inline seq

	and process_inline node =
		let comm = lazy (command_from_node node)
		in match node#node_type with
			| T_data			-> (!!comm, Ast.Plain node#data)
			| T_element "tex"		-> (!!comm, Ast.Mathtex_inl node#data)
			| T_element "math"		-> (!!comm, Ast.Mathml_inl node#data)
			| T_element "bold"
			| T_element "strong"
			| T_element "b"			-> (!!comm, Ast.Bold (process_seq node#sub_nodes))
			| T_element "emph"
			| T_element "em"
			| T_element "i"			-> (!!comm, Ast.Emph (process_seq node#sub_nodes))
			| T_element "mono"		-> (!!comm, Ast.Mono (process_seq node#sub_nodes))
			| T_element "caps"		-> (!!comm, Ast.Caps (process_seq node#sub_nodes))
			| T_element "thru"		-> (!!comm, Ast.Thru (process_seq node#sub_nodes))
			| T_element "sup"		-> (!!comm, Ast.Sup (process_seq node#sub_nodes))
			| T_element "sub"		-> (!!comm, Ast.Sub (process_seq node#sub_nodes))
			| T_element "mbox"		-> (!!comm, Ast.Mbox (process_seq node#sub_nodes))
			| T_element "link"
			| T_element "a"			-> (!!comm, Ast.Link (node#required_string_attribute "href", process_seq node#sub_nodes))
			| T_element "see"		-> (!!comm, Ast.See (node#required_string_attribute "href"))
			| T_element "cite"		-> (!!comm, Ast.Cite (node#required_string_attribute "href"))
			| T_element "ref"		-> (!!comm, Ast.Ref (node#required_string_attribute "href"))
			| T_element "sref"		-> (!!comm, Ast.Sref (node#required_string_attribute "href"))
			| T_element "mref"		-> (!!comm, Ast.Mref (node#required_string_attribute "href", process_seq node#sub_nodes))
			| _				-> failwith "process_inline_node"

	let rec process_frag frag =
		List.map process_block frag

	and process_block node =
		let comm = lazy (command_from_node node)
		in match node#node_type with
			| T_element "p"			-> (!!comm, Ast.Paragraph (process_seq node#sub_nodes))
			| T_element "itemize"
			| T_element "ul"		-> (!!comm, Ast.Itemize (List.map process_item node#sub_nodes))
			| T_element "enumerate"
			| T_element "ol"		-> (!!comm, Ast.Enumerate (List.map process_item node#sub_nodes))
			| T_element "description"
			| T_element "dl"		-> (!!comm, Ast.Description (process_definition_frag node#sub_nodes))
			| T_element "quote"		-> (!!comm, Ast.Quote [])
			| T_element "callout"		-> (!!comm, Ast.Callout (None, []))
			| T_element "code"		-> (!!comm, Ast.Code "")
			| T_element "tabular"		-> (!!comm, Ast.Tabular ("", {thead = None; tfoot = None; tbodies = []}))
			| T_element "verbatim"		-> (!!comm, Ast.Verbatim "")
			| T_element "bitmap"		-> (!!comm, Ast.Bitmap ("", ""))
			| T_element "subpage"		-> (!!comm, Ast.Subpage [])
			| T_element "equation"		-> let (block, caption) = process_wrapper node in (!!comm, Ast.Equation (caption, block))
			| T_element "printout"		-> let (block, caption) = process_wrapper node in (!!comm, Ast.Printout (caption, block))
			| T_element "table"		-> let (block, caption) = process_wrapper node in (!!comm, Ast.Table (caption, block))
			| T_element "figure"		-> let (block, caption) = process_wrapper node in (!!comm, Ast.Figure (caption, block))
			| T_element "part"		-> (!!comm, Ast.Part [])
			| T_element "appendix"		-> (!!comm, Ast.Appendix)
			| T_element "section"
			| T_element "h1"		-> (!!comm, Ast.Section (`Level1, []))
			| T_element "subsection"
			| T_element "h2"		-> (!!comm, Ast.Section (`Level2, []))
			| T_element "subsubsection"
			| T_element "h3"		-> (!!comm, Ast.Section (`Level3, []))
			| T_element "bibliography"	-> (!!comm, Ast.Bibliography)
			| T_element "notes"		-> (!!comm, Ast.Notes)
			| T_element "toc"		-> (!!comm, Ast.Toc)
			| T_element "title"		-> (!!comm, Ast.Title (`Level1, []))
			| T_element "subtitle"		-> (!!comm, Ast.Title (`Level2, []))
			| T_element "abstract"		-> (!!comm, Ast.Abstract [])
			| T_element "rule"		-> (!!comm, Ast.Rule)
			| T_element "bib"		-> let (who, what, where) = process_bib node#sub_nodes in (!!comm, Ast.Bib (who, what, where))
			| T_element "note"		-> (!!comm, Ast.Note [])
			| _				-> failwith "process_block_node"

	and process_item node =
		let comm = lazy (command_from_node node)
		in match node#node_type with
			| T_element "li"		-> (!!comm, process_frag node#sub_nodes)
			| _				-> failwith "process_item_node"

	and process_definition_frag frag =

		let pairify frag =
			let rec pairify_aux accum = function
				| dt::dd::tl	-> let new_accum = (dt, dd) :: accum in pairify_aux new_accum tl
				| []		-> accum
				| _		-> failwith "pairify"
			in pairify_aux [] frag in

		let process_definition_nodes (dt_node, dd_node) =
			let comm = lazy (command_from_node dt_node) in
			let (dt, dd) = match (dt_node#node_type, dd_node#node_type) with
				| (T_element "dt", T_element "dd")	-> (process_seq dt_node#sub_nodes, process_frag dd_node#sub_nodes)
				| _					-> failwith "process_definition_nodes"
			in (!!comm, dt, dd)

		in List.rev_map process_definition_nodes (pairify frag)

	and process_wrapper node = match node#sub_nodes with
		| [block_node; caption_node] when caption_node#node_type = T_element "caption" ->
			let block = process_block block_node
			and caption = (command_from_node caption_node, process_seq caption_node#sub_nodes)
			in (block, caption)
		| _ ->
			failwith "process_wrapper"

	and process_bib = function
		| [who_node; what_node; where_node] ->
			let who = (command_from_node who_node, process_seq who_node#sub_nodes)
			and what = (command_from_node what_node, process_seq what_node#sub_nodes)
			and where = (command_from_node where_node, process_seq where_node#sub_nodes)
			in (who, what, where)
		| _ ->
			failwith "process_bib"

	let process_document node = match node#node_type with
		| T_element "document" -> process_frag node#sub_nodes
		| _			-> failwith "unknown"

	let dtd =
		let config = {Pxp_types.default_config with Pxp_types.encoding = `Enc_utf8} in
		let source = Pxp_types.from_string (include_file "/home/dario/projects/lambdoc/trunk/lambdoc/src/read_lambhtml/lambhtml.dtd")
		in Pxp_dtd_parser.parse_dtd_entity config source

	let ast_from_string str =
		let config = {Pxp_types.default_config with Pxp_types.encoding = `Enc_utf8} in
		let spec = Pxp_tree_parser.default_spec in
		let source = Pxp_types.from_string ("<document>" ^ str ^ "</document>") in
		let tree = Pxp_tree_parser.parse_content_entity config source dtd spec
		in process_document tree#root
end

module M = Reader.Make_reader (R)

include M

