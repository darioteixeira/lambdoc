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

	let rec ast_from_seq seq =
		List.map ast_from_inline_node seq

	and ast_from_inline_node node =
		let comm = lazy (command_from_node node)
		in match node#node_type with
			| T_data			-> (!!comm, Ast.Plain node#data)
			| T_element "bold"
			| T_element "strong"
			| T_element "b"			-> (!!comm, Ast.Bold (ast_from_seq node#sub_nodes))
			| T_element "emph"
			| T_element "em"
			| T_element "i"			-> (!!comm, Ast.Emph (ast_from_seq node#sub_nodes))
			| T_element "mono"		-> (!!comm, Ast.Mono (ast_from_seq node#sub_nodes))
			| T_element "caps"		-> (!!comm, Ast.Caps (ast_from_seq node#sub_nodes))
			| T_element "thru"		-> (!!comm, Ast.Thru (ast_from_seq node#sub_nodes))
			| T_element "sup"		-> (!!comm, Ast.Sup (ast_from_seq node#sub_nodes))
			| T_element "sub"		-> (!!comm, Ast.Sub (ast_from_seq node#sub_nodes))
			| T_element "mbox"		-> (!!comm, Ast.Mbox (ast_from_seq node#sub_nodes))
			| T_element "link"
			| T_element "a"			-> (!!comm, Ast.Link (node#required_string_attribute "href", ast_from_seq node#sub_nodes))
			| T_element "see"		-> (!!comm, Ast.See (node#required_string_attribute "href"))
			| T_element "cite"		-> (!!comm, Ast.Cite (node#required_string_attribute "href"))
			| T_element "ref"		-> (!!comm, Ast.Ref (node#required_string_attribute "href"))
			| T_element "sref"		-> (!!comm, Ast.Sref (node#required_string_attribute "href"))
			| T_element "mref"		-> (!!comm, Ast.Mref (node#required_string_attribute "href", ast_from_seq node#sub_nodes))
			| _				-> failwith "ast_from_inline_node"

	let ast_from_block_node node =
		let comm = lazy (command_from_node node)
		in match node#node_type with
			| T_element "p"			-> (!!comm, Ast.Paragraph (ast_from_seq node#sub_nodes))
			| T_element "itemize"
			| T_element "ul"		-> (!!comm, Ast.Itemize [])
			| T_element "enumerate"
			| T_element "ol"		-> (!!comm, Ast.Enumerate [])
			| T_element "description"
			| T_element "dl"		-> (!!comm, Ast.Description [])
			| T_element "quote"		-> (!!comm, Ast.Quote [])
			| T_element "callout"		-> (!!comm, Ast.Callout (None, []))
			| T_element "code"		-> (!!comm, Ast.Code "")
			| T_element "tabular"		-> (!!comm, Ast.Tabular ("", {thead = None; tfoot = None; tbodies = []}))
			| T_element "verbatim"		-> (!!comm, Ast.Verbatim "")
			| T_element "bitmap"		-> (!!comm, Ast.Bitmap ("", ""))
			| T_element "subpage"		-> (!!comm, Ast.Subpage [])
			(*
			| T_element "equation"		-> (!!comm, Ast.Equation)
			| T_element "printout"		-> (!!comm, Ast.Printout)
			| T_element "table"		-> (!!comm, Ast.Table)
			| T_element "figure"		-> (!!comm, Ast.Figure)
			*)
			| T_element "part"		-> (!!comm, Ast.Part [])
			| T_element "appendix"		-> (!!comm, Ast.Appendix)
			| T_element "section"		-> (!!comm, Ast.Section (`Level1, []))
			| T_element "subsection"	-> (!!comm, Ast.Section (`Level2, []))
			| T_element "subsubsection"	-> (!!comm, Ast.Section (`Level3, []))
			| T_element "bibliography"	-> (!!comm, Ast.Bibliography)
			| T_element "notes"		-> (!!comm, Ast.Notes)
			| T_element "toc"		-> (!!comm, Ast.Toc)
			| T_element "title"		-> (!!comm, Ast.Title (`Level1, []))
			| T_element "subtitle"		-> (!!comm, Ast.Title (`Level2, []))
			| T_element "abstract"		-> (!!comm, Ast.Abstract [])
			| T_element "rule"		-> (!!comm, Ast.Rule)
			(*
			| T_element "bib"		-> (!!comm, Ast.Bib)
			*)
			| T_element "note"		-> (!!comm, Ast.Note [])
			| _				-> failwith "ast_from_block_node"

	let ast_from_document node = match node#node_type with
		| T_element "document" -> List.map ast_from_block_node node#sub_nodes
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
		in ast_from_document tree#root
end

module M = Reader.Make_reader (R)

include M

