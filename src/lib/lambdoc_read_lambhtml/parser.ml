(********************************************************************************)
(*	Parser.ml
	Copyright (c) 2009-2013 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Parser for the Lambhtml reader.
*)

open Pxp_document
open Pxp_types
open Lambdoc_core
open Lambdoc_reader
open Ast

module List = BatList
module String = BatString


(********************************************************************************)
(**	{1 Private modules}							*)
(********************************************************************************)

module Math_store =
struct
	type t = string BatDynArray.t

	let make () = BatDynArray.create ()

	let count_newlines str =
		let index from =
			try Some (String.index_from str from '\n')
			with _ -> None in
		let rec aux accum from = match index from with
			| Some x -> aux (accum+1) (x+1)
			| None	 -> accum
		in aux 0 0

	let add store subs =
		let (full, tag, attrs, data) = match Pcre.get_substrings ~full_match:true subs with
			| [| full; tag1; tag2; attrs; data |] -> (full, tag1 ^ tag2, attrs, data)
			| _				      -> failwith "Math_store.add" in
		let idx = BatDynArray.length store in
		let before = "<math" ^ tag ^ attrs ^ " idx=\"" ^ (string_of_int idx) ^ "\">"
		and newlines = String.make (count_newlines full) '\n'
		and after = "</math" ^ tag ^ ">" in
		BatDynArray.add store data;
		before ^ newlines ^ after

	let get store idx =
		BatDynArray.get store idx
end


(********************************************************************************)
(**	{1 Private functions and values}					*)
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
	"entity"; "br"; "mathtexinl"; "mathmlinl"; "glyph";
	"bold"; "strong"; "b"; "emph"; "em"; "i"; "code"; "tt"; "caps";
	"ins"; "del"; "sup"; "sub";
	"mbox"; "span"; "link"; "a"; "booklink";
	"see"; "cite"; "dref"; "sref"; "mref";
	"arg"; "call";
	]


let command_from_node node =
	{
	comm_tag = (match node#node_type with T_element tag -> Some tag | _ -> None);
	comm_label = node#optional_string_attribute "label";
	comm_order = node#optional_string_attribute "order";
	comm_style = node#optional_string_attribute "style";
	comm_linenum = let (_, line, _) = node#position in line - 1;
	}


let process_math =
	let expansion_map = [("&amp;", "&"); ("&apos;", "'"); ("&gt;", ">"); ("&lt;", "<"); ("&quot;", "\"")] in
	let subst str = try List.assoc str expansion_map with Not_found -> str in
	let rex = Pcre.regexp "&[a-z]+;" in
	fun ?(expand = false) store node ->
		let idx = int_of_string (node#required_string_attribute "idx") in
		let data = Math_store.get store idx in
		if expand
		then Pcre.substitute ~rex ~subst data
		else data


let rec process_seq store seq_root =
	List.map (process_inline store) seq_root#sub_nodes


and process_maybe_seq store seq_root = match seq_root#sub_nodes with
	| [] -> None
	| _  -> Some (process_seq store seq_root)


and process_macrocall store macro_root =
	let process_node node = match node#node_type with
		| T_element "with" -> process_seq store node
		| _		   -> failwith "process_macrocall"
	in List.map process_node macro_root#sub_nodes


and process_inline store node =
	let comm = lazy (command_from_node node) in
	match node#node_type with
		| T_data ->
			(!!comm, Ast.Plain node#data)
		| T_element "entity" ->
			(!!comm, Ast.Entity node#data)
		| T_element "br" ->
			(!!comm, Ast.Linebreak)
		| T_element "mathtexinl" ->
			let math = process_math ~expand:true store node in
			(!!comm, Ast.Mathtex_inl math)
		| T_element "mathmlinl" ->
			let math = process_math store node in
			(!!comm, Ast.Mathml_inl math)
		| T_element "glyph" ->
			let src = node#required_string_attribute "src" in
			let alt = node#required_string_attribute "alt" in
			(!!comm, Ast.Glyph (src, alt))
		| T_element "bold"
		| T_element "strong"
		| T_element "b" ->
			(!!comm, Ast.Bold (process_seq store node))
		| T_element "emph"
		| T_element "em"
		| T_element "i" ->
			(!!comm, Ast.Emph (process_seq store node))
		| T_element "code"
		| T_element "tt" ->
			(!!comm, Ast.Code (process_seq store node))
		| T_element "caps" ->
			(!!comm, Ast.Caps (process_seq store node))
		| T_element "ins" ->
			(!!comm, Ast.Ins (process_seq store node))
		| T_element "del" ->
			(!!comm, Ast.Del (process_seq store node))
		| T_element "sup" ->
			(!!comm, Ast.Sup (process_seq store node))
		| T_element "sub" ->
			(!!comm, Ast.Sub (process_seq store node))
		| T_element "mbox" ->
			(!!comm, Ast.Mbox (process_seq store node))
		| T_element "span" ->
			(!!comm, Ast.Span (process_seq store node))
		| T_element "link"
		| T_element "a" ->
			(!!comm, Ast.Link (node#required_string_attribute "href", process_maybe_seq store node))
		| T_element "booklink" ->
			(!!comm, Ast.Booklink (node#required_string_attribute "isbn", process_maybe_seq store node))
		| T_element "see" ->
			(!!comm, Ast.See (node#required_list_attribute "href"))
		| T_element "cite" ->
			(!!comm, Ast.Cite (node#required_list_attribute "href"))
		| T_element "dref" ->
			(!!comm, Ast.Dref (node#required_string_attribute "href", process_maybe_seq store node))
		| T_element "sref" ->
			(!!comm, Ast.Sref (node#required_string_attribute "href", process_maybe_seq store node))
		| T_element "mref" ->
			(!!comm, Ast.Mref (node#required_string_attribute "href", process_seq store node))
		| T_element "arg" ->
			(!!comm, Ast.Macroarg (node#required_string_attribute "num"))
		| T_element "call" ->
			(!!comm, Ast.Macrocall (node#required_string_attribute "name", process_macrocall store node))
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
	

let rec process_frag ?(flow = false) store frag_root =
	if flow then coalesce_flow frag_root;
	List.map (process_block store) frag_root#sub_nodes


and process_block store node =
	let comm = lazy (command_from_node node) in
	match node#node_type with
		| T_element "paragraph"
		| T_element "p" ->
			(!!comm, Ast.Paragraph (process_seq store node))
		| T_element "itemize"
		| T_element "ul" ->
			(!!comm, Ast.Itemize (process_anon_item_frag store node))
		| T_element "enumerate"
		| T_element "ol" ->
		(!!comm, Ast.Enumerate (process_anon_item_frag store node))
	| T_element "description"
		| T_element "dl" ->
			(!!comm, Ast.Description (process_desc_item_frag store node))
		| T_element "qanda" ->
			(!!comm, Ast.Qanda (process_qanda_frag store node))
		| T_element "verse" ->
			(!!comm, Ast.Verse (process_frag store node))
		| T_element "quote" ->
			(!!comm, Ast.Quote (process_frag store node))
		| T_element "mathtexblk" ->
			let math = process_math ~expand:true store node in
			(!!comm, Ast.Mathtex_blk math)
		| T_element "mathmlblk" ->
			let math = process_math store node in
			(!!comm, Ast.Mathml_blk math)
		| T_element "source" ->
			(!!comm, Ast.Source node#data)
		| T_element "tabular" ->
			let (cols, tabular) = process_tabular store node in
			(!!comm, Ast.Tabular (cols, tabular))
		| T_element "subpage" ->
			(!!comm, Ast.Subpage (process_frag store node))
		| T_element "verbatim"
		| T_element "pre" ->
			(!!comm, Ast.Verbatim node#data)
		| T_element "picture" ->
			let src = node#required_string_attribute "src" in
			let alt = node#required_string_attribute "alt" in
			(!!comm, Ast.Picture (src, alt))
		| T_element "bookpic" ->
			(!!comm, Ast.Bookpic (node#required_string_attribute "isbn"))
		| T_element "pull" ->
			(!!comm, Ast.Pullquote (None, process_frag store node))
		| T_element "boxout" ->
			let name = node#required_string_attribute "name" in
			let (maybe_caption, frag) = process_custom store node in
			(!!comm, Ast.Custom (Some Custom.Boxout, name, maybe_caption, frag))
		| T_element "theorem" ->
			let name = node#required_string_attribute "name" in
			let (maybe_caption, frag) = process_custom store node in
			(!!comm, Ast.Custom (Some Custom.Theorem, name, maybe_caption, frag))
		| T_element "equation" ->
			let (maybe_caption, block) = process_wrapper store node in
			(!!comm, Ast.Equation (maybe_caption, block))
		| T_element "printout" ->
			let (maybe_caption, block) = process_wrapper store node in
			(!!comm, Ast.Printout (maybe_caption, block))
		| T_element "table" ->
			let (maybe_caption, block) = process_wrapper store node in
			(!!comm, Ast.Table (maybe_caption, block))
		| T_element "figure" ->
			let (maybe_caption, block) = process_wrapper store node in
			(!!comm, Ast.Figure (maybe_caption, block))
		| T_element "part" ->
			(!!comm, Ast.Part (process_seq store node))
		| T_element "appendix" ->
			(!!comm, Ast.Appendix)
		| T_element "section"
		| T_element "h1" ->
			(!!comm, Ast.Section (`Level1, process_seq store node))
		| T_element "subsection"
		| T_element "h2" ->
			(!!comm, Ast.Section (`Level2, process_seq store node))
		| T_element "subsubsection"
		| T_element "h3" ->
			(!!comm, Ast.Section (`Level3, process_seq store node))
		| T_element "bibliography" ->
			(!!comm, Ast.Bibliography)
		| T_element "notes" ->
			(!!comm, Ast.Notes)
		| T_element "toc" ->
			(!!comm, Ast.Toc)
		| T_element "title" ->
			(!!comm, Ast.Title (`Level1, process_seq store node))
		| T_element "subtitle" ->
			(!!comm, Ast.Title (`Level2, process_seq store node))
		| T_element "abstract" ->
			(!!comm, Ast.Abstract (process_frag store node))
		| T_element "rule" ->
			(!!comm, Ast.Rule)
		| T_element "bib" ->
			(!!comm, Ast.Bib (process_bib store node))
		| T_element "note" ->
			(!!comm, Ast.Note (process_frag store node))
		| T_element "newmacro" ->
			let name = node#required_string_attribute "name" in
			let nargs = node#required_string_attribute "nargs" in
			(!!comm, Ast.Macrodef (name, nargs, process_seq store node))
		| T_element "newboxout" ->
			let name = node#required_string_attribute "name" in
			let maybe_caption = match node#sub_nodes with [] -> None | seq -> Some (process_seq store node) in
			let maybe_counter = node#optional_string_attribute "counter" in
			(!!comm, Ast.Boxoutdef (name, maybe_caption, maybe_counter))
		| T_element "newtheorem" ->
			let name = node#required_string_attribute "name" in
			let caption = process_seq store node in
			let maybe_counter = node#optional_string_attribute "counter" in
			(!!comm, Ast.Theoremdef (name, caption, maybe_counter))
		| _ ->
			failwith "process_block"


and process_anon_item_frag store frag_root=
	let process_node node =
		let comm = lazy (command_from_node node) in
		match node#node_type with
			| T_element "li" -> (!!comm, process_frag ~flow:true store node)
			| _		 -> failwith "process_anon_item_frag"
	in List.map process_node frag_root#sub_nodes


and process_desc_item_frag store frag_root =
	let process_nodes (dt_node, dd_node) =
		let comm = lazy (command_from_node dt_node) in
		let (dt, dd) = match (dt_node#node_type, dd_node#node_type) with
			| (T_element "dt", T_element "dd") -> (process_seq store dt_node, process_frag ~flow:true store dd_node)
			| _				   -> failwith "process_desc_item_frag"
		in (!!comm, dt, dd)
	in List.rev_map process_nodes (pairify frag_root#sub_nodes)


and process_qanda_frag store frag_root =
	let process_different cons node =
		let comm = lazy (command_from_node node) in
		match node#sub_nodes with
			| [dt; dd] -> (!!comm, cons (Some (process_seq store dt)), process_frag ~flow:true store dd)
			| [dd]	   -> (!!comm, cons None, process_frag ~flow:true store dd)
			| _	   -> failwith "process_different"
	and process_repeated cons node =
		let comm = lazy (command_from_node node) in
		(!!comm, cons, process_frag ~flow:true store node) in
	let process_node qora = match qora#node_type with
		| T_element "question"	-> process_different (fun maybe_seq -> New_questioner maybe_seq) qora
		| T_element "answer"	-> process_different (fun maybe_seq -> New_answerer maybe_seq) qora
		| T_element "rquestion"	-> process_repeated Same_questioner qora
		| T_element "ranswer)"	-> process_repeated Same_answerer qora
		| _			-> failwith "process_qanda_frag"
	in List.rev_map process_node frag_root#sub_nodes


and process_tabular store node =
	let cols = node#required_string_attribute "cols"
	and thead = ref None
	and tfoot = ref None
	and tbodies = ref [] in
	let process_cell node =
		let comm = lazy (command_from_node node) in
		let cellspec = node#optional_string_attribute "cell" in
		let maybe_seq = match process_seq store node with [] -> None | x -> Some x in
		(!!comm, cellspec, maybe_seq) in
	let process_row node =
		(command_from_node node, List.map process_cell node#sub_nodes) in
	let process_group node =
		let comm = lazy (command_from_node node) in
		match node#node_type with
			| T_element "thead" -> thead := Some (Some !!comm, List.map process_row node#sub_nodes)
			| T_element "tfoot" -> tfoot := Some (Some !!comm, List.map process_row node#sub_nodes)
			| T_element "tbody" -> tbodies := (Some !!comm, List.map process_row node#sub_nodes) :: !tbodies
			| _		    -> failwith "process_group" in
	List.iter process_group node#sub_nodes;
	(cols, {thead = !thead; tfoot = !tfoot; tbodies = List.rev !tbodies;})


and process_unifrag store node = match node#sub_nodes with
	| [blk] -> process_block store blk
	| _	-> failwith "process_unifrag"


and process_custom store node = match node#sub_nodes with
	| hd :: tl when hd#node_type = T_element "caption" ->
		node#remove_nodes ~pos:0 ~len:1 ();
		(Some (process_seq store hd), process_frag store node)
	| hd :: tl ->
		(None, process_frag store node)
	| _ ->
		failwith "process_custom"


and process_wrapper store node = match node#sub_nodes with
	| [caption_node; block_node] -> (Some (process_seq store caption_node), process_block store block_node)
	| [block_node]		     -> (None, process_block store block_node)
	| _			     -> failwith "process_wrapper"


and process_bib store node = match node#sub_nodes with
	| [who_node; what_node; where_node] ->
		let who = (command_from_node who_node, process_seq store who_node) in
		let what = (command_from_node what_node, process_seq store what_node) in
		let where = (command_from_node where_node, process_seq store where_node) in
		{author = who; title = what; resource = where}
	| _ ->
		failwith "process_bib"


let process_document store node = match node#node_type with
	| T_element "document" -> process_frag store node
	| _		       -> failwith "process_document"


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

let parse =
	let math_rex = Pcre.regexp ~flags:[`MULTILINE; `DOTALL] "<math(tex|ml)(inl|blk)\\b([^>]*?)>(.*?)</math\\1\\2>" in
	let entity_rex = Pcre.regexp "&(#?[a-zA-Z0-9]+);" in
	let entity_templ = Pcre.subst "<entity>$1</entity>" in
	fun str ->
		let store = Math_store.make () in
		let str = Pcre.substitute_substrings ~rex:math_rex ~subst:(Math_store.add store) str in
		let str = Pcre.replace ~rex:entity_rex ~itempl:entity_templ str in
		let config =
			{
			default_config with
			encoding = `Enc_utf8;
			idref_pass = false;
			} in
		let spec = Pxp_tree_parser.default_spec in
		let source = Pxp_types.from_string ("<document>\n" ^ str ^ "</document>") in
		let tree = Pxp_tree_parser.parse_content_entity config source Dtd.lambhtml_dtd spec in
		process_document store tree#root

