(********************************************************************************)
(*	Parser.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Parser for the Lambhtml reader.
*)

open Pxp_document
open Pxp_types
open Lambdoc_reader
open Ast


(********************************************************************************)
(*	{2 Private functions and values}					*)
(********************************************************************************)

let pairify lst =
	let rec pairify_aux accum = function
		| one::two::tl	-> let new_accum = (one, two) :: accum in pairify_aux new_accum tl
		| []		-> accum
		| _		-> invalid_arg "pairify"
	in pairify_aux [] lst


let (!!) = Lazy.force


let inline_elems =
	[
	"br"; "mathtexinl"; "mathmlinl";
	"bold"; "strong"; "b"; "emph"; "em"; "i"; "code"; "tt"; "caps";
	"ins"; "del"; "sup"; "sub";
	"mbox"; "link"; "a";
	"see"; "cite"; "ref"; "sref"; "mref";
	"arg"; "call";
	]


let command_from_node node =
	{
	comm_tag = (match node#node_type with T_element tag -> Some tag | _ -> None);
	comm_label = node#optional_string_attribute "label";
	comm_order = node#optional_string_attribute "order";
	comm_extra = node#optional_string_attribute "extra";
	comm_linenum = let (_, line, _) = node#position in line - 1;
	}


let rec process_seq seq_root =
	List.map process_inline seq_root#sub_nodes


and process_maybe_seq seq_root = match seq_root#sub_nodes with
	| []	-> None
	| _	-> Some (process_seq seq_root)


and process_macrocall macro_root =
	let process_node node = match node#node_type with
		| T_element "with" -> process_seq node
		| _		   -> failwith "process_macrocall"
	in List.map process_node macro_root#sub_nodes


and process_inline node =
	let comm = lazy (command_from_node node)
	in match node#node_type with
		| T_data ->
			(!!comm, Ast.Plain node#data)
		| T_element "br" ->
			(!!comm, Ast.Linebreak)
		| T_element "mathtexinl" ->
			(!!comm, Ast.Mathtex_inl node#data)
		| T_element "mathmlinl" ->
			(!!comm, Ast.Mathml_inl node#data)
		| T_element "bold"
		| T_element "strong"
		| T_element "b" ->
			(!!comm, Ast.Bold (process_seq node))
		| T_element "emph"
		| T_element "em"
		| T_element "i" ->
			(!!comm, Ast.Emph (process_seq node))
		| T_element "code"
		| T_element "tt" ->
			(!!comm, Ast.Code (process_seq node))
		| T_element "caps" ->
			(!!comm, Ast.Caps (process_seq node))
		| T_element "ins" ->
			(!!comm, Ast.Ins (process_seq node))
		| T_element "del" ->
			(!!comm, Ast.Del (process_seq node))
		| T_element "sup" ->
			(!!comm, Ast.Sup (process_seq node))
		| T_element "sub" ->
			(!!comm, Ast.Sub (process_seq node))
		| T_element "mbox" ->
			(!!comm, Ast.Mbox (process_seq node))
		| T_element "link"
		| T_element "a" ->
			(!!comm, Ast.Link (node#required_string_attribute "href", process_maybe_seq node))
		| T_element "see" ->
			(!!comm, Ast.See (node#required_list_attribute "href"))
		| T_element "cite" ->
			(!!comm, Ast.Cite (node#required_list_attribute "href"))
		| T_element "ref" ->
			(!!comm, Ast.Ref (node#required_string_attribute "href"))
		| T_element "sref" ->
			(!!comm, Ast.Sref (node#required_string_attribute "href"))
		| T_element "mref" ->
			(!!comm, Ast.Mref (node#required_string_attribute "href", process_seq node))
		| T_element "arg" ->
			(!!comm, Ast.Macroarg (node#required_string_attribute "num"))
		| T_element "call" ->
			(!!comm, Ast.Macrocall (node#required_string_attribute "name", process_macrocall node))
		| _ ->
			failwith "process_inline"


(*	In flow mode we must place loose inline elements into newly created paragraph blocks.
*)
let coalesce_flow : _ Pxp_document.node -> unit = fun frag_root ->
	let current_paragraph = ref None in
	let move_node node = match !current_paragraph with
		| None ->
			let paragraph = Pxp_document.create_element_node Pxp_tree_parser.default_spec node#dtd "p" [] in
			frag_root#insert_nodes ~pos:node#node_position [paragraph];
			current_paragraph := Some paragraph;
			node#remove ();
			paragraph#append_node node
		| Some paragraph ->
			node#remove ();
			paragraph#append_node node in
	let check_node node = match node#node_type with
		| T_data -> move_node node
		| T_element elem when List.mem elem inline_elems -> move_node node
		| _ -> current_paragraph := None
	in List.iter check_node frag_root#sub_nodes
	

let rec process_frag ?(flow = false) frag_root =
	if flow then coalesce_flow frag_root;
	List.map process_block frag_root#sub_nodes


and process_block node =
	let comm = lazy (command_from_node node)
	in match node#node_type with
		| T_element "paragraph"
		| T_element "p" ->
			(!!comm, Ast.Paragraph (process_seq node))
		| T_element "itemize"
		| T_element "ul" ->
			(!!comm, Ast.Itemize (process_anon_item_frag node))
		| T_element "enumerate"
		| T_element "ol" ->
			(!!comm, Ast.Enumerate (process_anon_item_frag node))
		| T_element "description"
		| T_element "dl" ->
			(!!comm, Ast.Description (process_desc_item_frag node))
		| T_element "qanda" ->
			(!!comm, Ast.Qanda (process_qanda_frag node))
		| T_element "verse" ->
			(!!comm, Ast.Verse (process_frag node))
		| T_element "quote" ->
			(!!comm, Ast.Quote (process_frag node))
		| T_element "mathtexblk" ->
			(!!comm, Ast.Mathtex_blk node#data)
		| T_element "mathmlblk" ->
			let buffer = Buffer.create 16 in
			let display_node : _ Pxp_document.node -> unit = fun node -> node#display (`Out_buffer buffer) `Enc_utf8 in
			let () = node#iter_nodes display_node
			in (!!comm, Ast.Mathml_blk (Buffer.contents buffer))
		| T_element "source" ->
			(!!comm, Ast.Source node#data)
		| T_element "tabular" ->
			let (cols, tabular) = process_tabular node
			in (!!comm, Ast.Tabular (cols, tabular))
		| T_element "verbatim"
		| T_element "pre" ->
			(!!comm, Ast.Verbatim node#data)
		| T_element "image" ->
			let src = node#required_string_attribute "src"
			and alt = node#required_string_attribute "alt"
			in (!!comm, Ast.Image (src, alt))
		| T_element "subpage" ->
			(!!comm, Ast.Subpage (process_frag node))
		| T_element "decor" ->
			(!!comm, Ast.Decor (process_unifrag node))
		| T_element "pull" ->
			(!!comm, Ast.Pullquote (None, process_frag node))
		| T_element "boxout"
		| T_element "theorem" ->
			let name = node#required_string_attribute "name"
			and (maybe_caption, frag) = process_custom node
			in (!!comm, Ast.Custom (name, maybe_caption, frag))
		| T_element "equation" ->
			let (maybe_caption, block) = process_wrapper node
			in (!!comm, Ast.Equation (maybe_caption, block))
		| T_element "printout" ->
			let (maybe_caption, block) = process_wrapper node
			in (!!comm, Ast.Printout (maybe_caption, block))
		| T_element "table" ->
			let (maybe_caption, block) = process_wrapper node
			in (!!comm, Ast.Table (maybe_caption, block))
		| T_element "figure" ->
			let (maybe_caption, block) = process_wrapper node
			in (!!comm, Ast.Figure (maybe_caption, block))
		| T_element "part" ->
			(!!comm, Ast.Part (process_seq node))
		| T_element "appendix" ->
			(!!comm, Ast.Appendix)
		| T_element "section"
		| T_element "h1" ->
			(!!comm, Ast.Section (`Level1, process_seq node))
		| T_element "subsection"
		| T_element "h2" ->
			(!!comm, Ast.Section (`Level2, process_seq node))
		| T_element "subsubsection"
		| T_element "h3" ->
			(!!comm, Ast.Section (`Level3, process_seq node))
		| T_element "bibliography" ->
			(!!comm, Ast.Bibliography)
		| T_element "notes" ->
			(!!comm, Ast.Notes)
		| T_element "toc" ->
			(!!comm, Ast.Toc)
		| T_element "title" ->
			(!!comm, Ast.Title (`Level1, process_seq node))
		| T_element "subtitle" ->
			(!!comm, Ast.Title (`Level2, process_seq node))
		| T_element "abstract" ->
			(!!comm, Ast.Abstract (process_frag node))
		| T_element "rule" ->
			(!!comm, Ast.Rule)
		| T_element "bib" ->
			(!!comm, Ast.Bib (process_bib node))
		| T_element "note" ->
			(!!comm, Ast.Note (process_frag node))
		| T_element "newmacro" ->
			let name = node#required_string_attribute "name"
			and nargs = node#required_string_attribute "nargs"
			in (!!comm, Ast.Macrodef (name, nargs, process_seq node))
		| T_element "newboxout" ->
			let name = node#required_string_attribute "name"
			and maybe_caption = match node#sub_nodes with [] -> None | seq -> Some (process_seq node)
			and maybe_counter = node#optional_string_attribute "counter"
			in (!!comm, Ast.Boxoutdef (name, maybe_caption, maybe_counter))
		| T_element "newtheorem" ->
			let name = node#required_string_attribute "name"
			and caption = process_seq node
			and maybe_counter = node#optional_string_attribute "counter"
			in (!!comm, Ast.Theoremdef (name, caption, maybe_counter))
		| T_element x ->
			prerr_endline ("### " ^ x ^ " ###");
			failwith "process_block"
		| _ ->
			failwith "process_block"


and process_anon_item_frag frag_root=
	let process_node node =
		let comm = lazy (command_from_node node)
		in match node#node_type with
			| T_element "li" -> (!!comm, process_frag ~flow:true node)
			| _		 -> failwith "process_anon_item_frag"
	in List.map process_node frag_root#sub_nodes


and process_desc_item_frag frag_root =
	let process_nodes (dt_node, dd_node) =
		let comm = lazy (command_from_node dt_node) in
		let (dt, dd) = match (dt_node#node_type, dd_node#node_type) with
			| (T_element "dt", T_element "dd") -> (process_seq dt_node, process_frag ~flow:true dd_node)
			| _				   -> failwith "process_desc_item_frag"
		in (!!comm, dt, dd)
	in List.rev_map process_nodes (pairify frag_root#sub_nodes)


and process_qanda_frag frag_root =
	let process_different node =
		let comm = lazy (command_from_node node)
		in match node#sub_nodes with
			| [dt; dd] -> (!!comm, Different (Some (process_seq dt)), process_frag ~flow:true dd)
			| [dd]	   -> (!!comm, Different None, process_frag ~flow:true dd)
			| _	   -> failwith "process_different"
	and process_repeated node =
		let comm = lazy (command_from_node node)
		in (!!comm, Repeated, process_frag ~flow:true node) in
	let process_group (question, answer) = match (question#node_type, answer#node_type) with
		| (T_element "question", T_element "answer")	-> (process_different question, process_different answer)
		| (T_element "question", T_element "ranswer")	-> (process_different question, process_repeated answer)
		| (T_element "rquestion", T_element "answer")	-> (process_repeated question, process_different answer)
		| (T_element "rquestion", T_element "ranswer")	-> (process_repeated question, process_repeated answer)
		| _						-> failwith "process_qanda_frag"
	in List.rev_map process_group (pairify frag_root#sub_nodes)


and process_tabular node =
	let cols = node#required_string_attribute "cols"
	and thead = ref None
	and tfoot = ref None
	and tbodies = ref [] in
	let process_cell node =
		let comm = lazy (command_from_node node)
		and cellspec = node#optional_string_attribute "cell"
		and maybe_seq = match process_seq node with [] -> None | x -> Some x
		in (!!comm, cellspec, maybe_seq) in
	let process_row node =
		(command_from_node node, List.map process_cell node#sub_nodes) in
	let process_group node =
		let comm = lazy (command_from_node node)
		in match node#node_type with
			| T_element "thead" -> thead := Some (Some !!comm, List.map process_row node#sub_nodes)
			| T_element "tfoot" -> tfoot := Some (Some !!comm, List.map process_row node#sub_nodes)
			| T_element "tbody" -> tbodies := (Some !!comm, List.map process_row node#sub_nodes) :: !tbodies
			| _		    -> failwith "process_group" in
	let () = List.iter process_group node#sub_nodes
	in (cols, {thead = !thead; tfoot = !tfoot; tbodies = List.rev !tbodies;})


and process_unifrag node = match node#sub_nodes with
	| [blk] -> process_block blk
	| _	-> failwith "process_unifrag"


and process_custom node = match node#sub_nodes with
	| hd :: tl when hd#node_type = T_element "caption" ->
		node#remove_nodes ~pos:0 ~len:1 ();
		(Some (process_seq hd), process_frag node)
	| hd :: tl ->
		(None, process_frag node)
	| _ ->
		failwith "process_custom"


and process_wrapper node = match node#sub_nodes with
	| [caption_node; block_node] -> (Some (process_seq caption_node), process_block block_node)
	| [block_node]		     -> (None, process_block block_node)
	| _			     -> failwith "process_wrapper"


and process_bib node = match node#sub_nodes with
	| [who_node; what_node; where_node] ->
		let who = (command_from_node who_node, process_seq who_node)
		and what = (command_from_node what_node, process_seq what_node)
		and where = (command_from_node where_node, process_seq where_node)
		in {author = who; title = what; resource = where}
	| _ ->
		failwith "process_bib"


let process_document node = match node#node_type with
	| T_element "document" -> process_frag node
	| _		       -> failwith "process_document"


(********************************************************************************)
(*	{2 Public functions and values}						*)
(********************************************************************************)

let parse str =
	let warner = object method warn w = print_endline ("WARNING: " ^ w) end in
	let config =
		{
		default_config with
		encoding = `Enc_utf8;
		idref_pass = false;
		enable_namespace_processing = None;
		warner = warner;
		} in
	let spec = Pxp_tree_parser.default_spec in
	let source = Pxp_types.from_string ("<document>\n" ^ str ^ "</document>") in
	let tree = Pxp_tree_parser.parse_content_entity config source Dtd.lambhtml_dtd spec
	in process_document tree#root

