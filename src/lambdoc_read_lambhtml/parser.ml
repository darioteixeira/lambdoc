(********************************************************************************)
(*	Parser.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed nestable the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Parser for the Lambhtml reader.
*)

open Pxp_document
open Lambdoc_reader
open Ast


let (!!) = Lazy.force

let command_from_node node =
	{
	comm_tag = (match node#node_type with T_element tag -> Some tag | _ -> None);
	comm_label = node#optional_string_attribute "label";
	comm_order = node#optional_string_attribute "order";
	comm_extra = node#optional_string_attribute "extra";
	comm_linenum = let (_, line, _) = node#position in line - 1;
	}

let rec process_seq seq =
	List.map process_inline seq

and process_inline node =
	let comm = lazy (command_from_node node)
	in match node#node_type with
		| T_data			-> (!!comm, Ast.Plain node#data)
		| T_element "br"		-> (!!comm, Ast.Break)
		| T_element "bold"
		| T_element "strong"
		| T_element "b"			-> (!!comm, Ast.Bold (process_seq node#sub_nodes))
		| T_element "emph"
		| T_element "em"
		| T_element "i"			-> (!!comm, Ast.Emph (process_seq node#sub_nodes))
		| T_element "mono"
		| T_element "tt"		-> (!!comm, Ast.Mono (process_seq node#sub_nodes))
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

let rec process_frag ?(flow = false) frag =
	if flow
	then begin	(* In flow mode we must place loose inline elements into newly created paragraph blocks. *)
	end
	else ();
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
		| T_element "pull"		-> (!!comm, Ast.Pullquote (process_frag node#sub_nodes))
		| T_element "boxout"		-> let (msg, frag) = process_callout node in (!!comm, Ast.Boxout (msg, frag))
		| T_element "code"		-> (!!comm, Ast.Code node#data)
		| T_element "tabular"		-> let (cols, tabular) = process_tabular node in (!!comm, Ast.Tabular (cols, tabular))
		| T_element "verbatim"
		| T_element "pre"		-> (!!comm, Ast.Verbatim node#data)
		| T_element "bitmap"		-> (!!comm, Ast.Bitmap (node#required_string_attribute "src", node#required_string_attribute "alt"))
		| T_element "subpage"		-> (!!comm, Ast.Subpage (process_frag node#sub_nodes))
		| T_element "equation"		-> let (block, caption) = process_wrapper node in (!!comm, Ast.Equation (caption, block))
		| T_element "printout"		-> let (block, caption) = process_wrapper node in (!!comm, Ast.Printout (caption, block))
		| T_element "table"		-> let (block, caption) = process_wrapper node in (!!comm, Ast.Table (caption, block))
		| T_element "figure"		-> let (block, caption) = process_wrapper node in (!!comm, Ast.Figure (caption, block))
		| T_element "part"		-> (!!comm, Ast.Part (process_seq node#sub_nodes))
		| T_element "appendix"		-> (!!comm, Ast.Appendix)
		| T_element "section"
		| T_element "h1"		-> (!!comm, Ast.Section (`Level1, process_seq node#sub_nodes))
		| T_element "subsection"
		| T_element "h2"		-> (!!comm, Ast.Section (`Level2, process_seq node#sub_nodes))
		| T_element "subsubsection"
		| T_element "h3"		-> (!!comm, Ast.Section (`Level3, process_seq node#sub_nodes))
		| T_element "bibliography"	-> (!!comm, Ast.Bibliography)
		| T_element "notes"		-> (!!comm, Ast.Notes)
		| T_element "toc"		-> (!!comm, Ast.Toc)
		| T_element "title"		-> (!!comm, Ast.Title (`Level1, process_seq node#sub_nodes))
		| T_element "subtitle"		-> (!!comm, Ast.Title (`Level2, process_seq node#sub_nodes))
		| T_element "abstract"		-> (!!comm, Ast.Abstract [])
		| T_element "rule"		-> (!!comm, Ast.Rule)
		| T_element "bib"		-> (!!comm, Ast.Bib (process_bib node#sub_nodes))
		| T_element "note"		-> (!!comm, Ast.Note (process_frag node#sub_nodes))
		| _				-> failwith "process_block_node"

and process_item node =
	let comm = lazy (command_from_node node)
	in match node#node_type with
		| T_element "li"		-> (!!comm, process_frag ~flow:true node#sub_nodes)
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
			| (T_element "dt", T_element "dd")	-> (process_seq dt_node#sub_nodes, process_frag ~flow:true dd_node#sub_nodes)
			| _					-> failwith "process_definition_nodes"
		in (!!comm, dt, dd)

	in List.rev_map process_definition_nodes (pairify frag)

and process_callout node = match node#sub_nodes with
	| msg_node :: frag when msg_node#node_type = T_element "msg" ->
		(Some (process_seq msg_node#sub_nodes), process_frag frag)
	| frag ->
		(None, process_frag frag)

and process_tabular node =
	let cols = node#required_string_attribute "cols"
	and thead = ref None
	and tfoot = ref None
	and tbodies = ref [] in
	let process_col node =
		process_seq node#sub_nodes in
	let process_row node =
		(command_from_node node, List.map process_col node#sub_nodes) in
	let process_group node =
		let comm = lazy (command_from_node node)
		in match node#node_type with
			| T_element "thead"	-> thead := Some (Some !!comm, List.map process_row node#sub_nodes)
			| T_element "tfoot"	-> tfoot := Some (Some !!comm, List.map process_row node#sub_nodes)
			| T_element "tbody"	-> tbodies := (Some !!comm, List.map process_row node#sub_nodes) :: !tbodies
			| _			-> failwith "process_group" in
	let () = List.iter process_group node#sub_nodes
	in (cols, {thead = !thead; tfoot = !tfoot; tbodies = List.rev !tbodies;})

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
		in {author = who; title = what; resource = where}
	| _ ->
		failwith "process_bib"

let process_document node = match node#node_type with
	| T_element "document" -> process_frag node#sub_nodes
	| _			-> failwith "process_document"

let parse str =
	let config =
		{
		Pxp_types.default_config with
		Pxp_types.encoding = `Enc_utf8;
		Pxp_types.idref_pass = false;
		Pxp_types.enable_namespace_processing = None;
		} in
	let spec = Pxp_tree_parser.default_spec in
	let source = Pxp_types.from_string ("<document>\n" ^ str ^ "</document>") in
	let tree = Pxp_tree_parser.parse_content_entity config source Dtd.lambhtml_dtd spec
	in process_document tree#root

