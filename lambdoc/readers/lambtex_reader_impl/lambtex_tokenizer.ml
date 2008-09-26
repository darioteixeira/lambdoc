(********************************************************************************)
(*	Tokenizer for the Lambtex reader.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed nestable the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lexing
open ExtString
open Document_ast.Ast
open Lambtex_parser
open Lambtex_scanner


(********************************************************************************)
(**	{2 Exceptions}								*)
(********************************************************************************)

exception Unknown_env_command of string
exception Unknown_simple_command of string


(********************************************************************************)
(**	{2 Type declarations}							*)
(********************************************************************************)

(**	Scanning environments determine which scanner will be invoked.
*)
type scanner_environment_t =
	| General
	| Tabular
	| Code
	| Verbatim
	| Mathtex_inl
	| Mathml_inl
	| Mathtex_blk
	| Mathml_blk


(**	Scanning contexts control the way that whitespace is handled.
*)
type scanner_context_t =
	| Block
	| Inline
	| Raw


(**	Actions that control current scanning environment or context.
*)
type 'a action_t =
	| Set of 'a
	| Push of 'a
	| Pop
	| Keep


(**	The tokenizer will take each scanner token and place it into
	one of five different categories.  This helps the coalescing
	method to group several contiguous tokens of the same category.
*)
type scanner_category_t =
	| Cat_token of Lambtex_parser.token
	| Cat_begin of Lambtex_parser.token
	| Cat_end of Lambtex_parser.token
	| Cat_space
	| Cat_ignore


(********************************************************************************)
(**	{2 Tag-building functions}						*)
(********************************************************************************)

(**	This function returns the tag information corresponding to the string form
	of the supplied environment tag.  An environment tag is one that begins with
	a \begin declaration and ends with a corresponding \end.  The information
	returned is a pair consisting of the parser token and the scanning context.

*)
let get_env_tag params is_begin =
	let (token_begin, token_end, new_env, context_begin, context_end) = match params.comm_tag with
	| "abstract"	-> (BEGIN_ABSTRACT params, END_ABSTRACT params,		General,	Set Block,	Set Block) 
	| "itemize"	-> (BEGIN_ITEMIZE params, END_ITEMIZE params,		General,	Set Block,	Set Block) 
	| "enumerate"	-> (BEGIN_ENUMERATE params, END_ENUMERATE params,	General,	Set Block,	Set Block) 
	| "quote"	-> (BEGIN_QUOTE params,	END_QUOTE params,		General,	Set Block,	Set Block)
	| "tex"		-> (BEGIN_MATHTEX_BLK params, END_MATHTEX_BLK params,	Mathtex_blk,	Set Raw,	Set Block)
	| "mathml"	-> (BEGIN_MATHML_BLK params, END_MATHML_BLK params,	Mathml_blk,	Set Raw,	Set Block)
	| "code"	-> (BEGIN_CODE params, END_CODE params,			Code,		Set Raw,	Set Block)
	| "verbatim"	-> (BEGIN_VERBATIM params, END_VERBATIM params,		Verbatim,	Set Raw,	Set Block) 
	| "tabular"	-> (BEGIN_TABULAR params, END_TABULAR params,		Tabular,	Set Block,	Set Block) 
	| "subpage"	-> (BEGIN_SUBPAGE params, END_SUBPAGE params,		General,	Set Block,	Set Block) 
	| "equation"	-> (BEGIN_EQUATION params, END_EQUATION params,		General,	Set Block,	Set Block) 
	| "algorithm"	-> (BEGIN_ALGORITHM params, END_ALGORITHM params,	General,	Set Block,	Set Block) 
	| "table"	-> (BEGIN_TABLE params, END_TABLE params,		General,	Set Block,	Set Block) 
	| "figure"	-> (BEGIN_FIGURE params, END_FIGURE params,		General,	Set Block,	Set Block) 
	| "bib"		-> (BEGIN_BIB params, END_BIB params,			General,	Set Block,	Set Block) 
	| other		-> raise (Unknown_env_command other)

	in if is_begin
	then (Cat_token token_begin, Push new_env, context_begin)
	else (Cat_token token_end, Pop, context_end)


(**	This function returns the tag information corresponding to the string
	form of the supplied simple tag.  A simple tag is one whose begin is
	signaled by its own escaped name.  The information returned is a pair
	consisting of the parser token and the tokenizer filter.
*)
let get_simple_tag params =
	let (token, context) =
		match params.comm_tag with
		| "bold"		-> (BOLD params,		Set Inline)
		| "emph"		-> (EMPH params,		Set Inline)
		| "mono"		-> (MONO params,		Set Inline)
		| "caps"		-> (CAPS params,		Set Inline)
		| "thru"		-> (THRU params,		Set Inline)
		| "sup"			-> (SUP params,			Set Inline)
		| "sub"			-> (SUB params,			Set Inline)
		| "box"			-> (BOX params,			Set Inline)

		| "link"		-> (LINK params,		Set Inline)
		| "see"			-> (SEE params,			Set Inline)
		| "cite"		-> (CITE params,		Set Inline)
		| "ref"			-> (REF params,			Set Inline)
		| "sref"		-> (SREF params,		Set Inline)
		| "mref"		-> (MREF params,		Set Inline)

		| "section"		-> (SECTION params,		Set Block)
		| "subsection"		-> (SUBSECTION params,		Set Block)
		| "subsubsection"	-> (SUBSUBSECTION params,	Set Block)
		| "toc"			-> (TOC params,			Set Block)
		| "bibliography"	-> (BIBLIOGRAPHY params,	Set Block)
		| "notes"		-> (NOTES params,		Set Block)

		| "title"		-> (TITLE params, 		Set Block)
		| "rule"		-> (RULE params,		Set Block)
		| "appendix"		-> (APPENDIX params,		Set Block)
		| "set"			-> (SETTING params,		Set Block)
		| "item"		-> (NEW_ITEM params,		Set Block)
		| "image"		-> (IMAGE params,		Set Block)
		| "caption"		-> (CAPTION params,		Set Block)
		| "head"		-> (HEAD params,		Set Block)
		| "foot"		-> (FOOT params,		Set Block)
		| "body"		-> (BODY params,		Set Block)
		| "btitle"		-> (BIB_TITLE params,		Set Block)
		| "bauthor"		-> (BIB_AUTHOR params,		Set Block)
		| "bresource"		-> (BIB_RESOURCE params,	Set Block)
		| "note"		-> (NOTE params,		Set Block)

		| other			-> raise (Unknown_simple_command other)

	in (Cat_token token, Keep, context)


(********************************************************************************)
(**	{2 Functions to process commands and parameters}			*)
(********************************************************************************)

let pat_env = "\\\\(?<env>(begin)|(end))"
let pat_command = "\\\\(?<command>\\w+)"
let pat_primary = "\\{(?<primary>\\w+)\\}"
let pat_secondary = "(?<secondary>\\{\\w*\\})?"

let pat_order = "(?<order>\\([\\.\\d]*\\))"
let pat_label = "(?<label>\\[[\\w\\d\\-:_]*\\])"
let pat_extra = "(?<extra><[\\w\\d]*>)"
let pat_optional = "(" ^ pat_order ^ "|" ^ pat_extra ^ "|" ^ pat_label ^")*"


(**	Processes raw parameters.  Basically it determines whether the parameter
	actually exists, and if so, removes the leading and trailing marker.
*)
let get_param rex name subs =
	try
		let res = Pcre.get_named_substring rex name subs
		in Some (String.slice ~first:1 ~last:(-1) res)
	with
		_ -> None


(**	Builds a command_t.
*)
let build_command lexbuf tag rex subs =
	{
	comm_tag = tag;
	comm_label = get_param rex "label" subs;
	comm_order = get_param rex "order" subs;
	comm_extra = get_param rex "extra" subs;
	comm_secondary = get_param rex "secondary" subs;
	comm_linenum = lexbuf.lex_curr_p.pos_lnum;
	}


(**	Builds a operator_t.
*)
let build_op lexbuf =
	{
	op_linenum = lexbuf.lex_curr_p.pos_lnum;
	}


(**	Issues an environment command.
*)
let issue_env_command =
	let rex = Pcre.regexp ("^" ^ pat_env ^ pat_optional ^ pat_primary ^ pat_secondary ^ "$") in
	fun lexbuf ->
		let subs = Pcre.exec ~rex (Lexing.lexeme lexbuf) in
		let command = Pcre.get_named_substring rex "env" subs
		and primary = Pcre.get_named_substring rex "primary" subs in
		let params = build_command lexbuf primary rex subs in
		get_env_tag params (command = "begin")


(**	Issues a simple command.
*)
let issue_simple_command =
	let rex = Pcre.regexp ("^" ^ pat_command ^ pat_optional ^ "$") in
	fun lexbuf ->
		let subs = Pcre.exec ~rex (Lexing.lexeme lexbuf) in
		let command = Pcre.get_named_substring rex "command" subs in
		let params = build_command lexbuf command rex subs in
		get_simple_tag params


(********************************************************************************)
(**	{2 Tokenizer class}							*)
(********************************************************************************)

class tokenizer =
object (self)

	(**	Mutable values.
	*)

	val mutable current_env = General
	val mutable env_history = Stack.create ()
	val mutable current_context = Block
	val mutable context_history = Stack.create ()
	val mutable productions = Queue.create ()


	(**	Adds a new element to the queue of productions.
	*)
	method store elem =
		Queue.add elem productions


	(**	Coalesces the queue of productions.  This means that consecutive
		"PLAIN" productions (which consist of individual characters) are
		strung together into a single string.
	*)
	method coalesce lexbuf =
		let rec taker accum =
			try
				match Queue.peek productions with
				| PLAIN text	-> ignore (Queue.take productions); taker (accum ^ text)
				| other		-> PLAIN accum
			with
				| Queue.Empty -> self#produce lexbuf; taker accum
		in try
			match Queue.take productions with
			| PLAIN text	-> taker text
			| other		-> other
		with
			| Queue.Empty -> self#produce lexbuf; self#coalesce lexbuf



	(**	Invokes the scanner to produces a new token.
	*)
	method produce lexbuf =

		let scanner = match current_env with
			| General	-> Lambtex_scanner.general_scanner
			| Tabular	-> Lambtex_scanner.tabular_scanner
			| Code		-> Lambtex_scanner.code_scanner
			| Verbatim	-> Lambtex_scanner.verbatim_scanner
			| Mathtex_inl	-> Lambtex_scanner.mathtex_inl_scanner
			| Mathml_inl	-> Lambtex_scanner.mathml_inl_scanner
			| Mathtex_blk	-> Lambtex_scanner.mathtex_blk_scanner
			| Mathml_blk	-> Lambtex_scanner.mathml_blk_scanner in

		let (category, potential_new_env, potential_new_context) = match scanner lexbuf with
			| Tok_env_command buf		-> issue_env_command buf
			| Tok_simple_command buf	-> issue_simple_command buf
			| Tok_begin buf			-> (Cat_begin (BEGIN (build_op buf)),			Keep, Push Inline)
			| Tok_end buf			-> (Cat_end (END (build_op buf)),			Keep, Pop)
			| Tok_begin_mathtex_inl buf	-> (Cat_token (BEGIN_MATHTEX_INL (build_op buf)),	Push Mathtex_inl, Push Raw)
			| Tok_end_mathtex_inl buf	-> (Cat_token (END_MATHTEX_INL (build_op buf)),		Pop, Pop)
			| Tok_begin_mathml_inl buf	-> (Cat_token (BEGIN_MATHML_INL (build_op buf)),	Push Mathml_inl, Push Raw)
			| Tok_end_mathml_inl buf	-> (Cat_token (END_MATHML_INL (build_op buf)),		Pop, Pop)
			| Tok_eof buf			-> (Cat_token (EOF (build_op buf)),			Keep, Keep)
			| Tok_column_sep buf		-> (Cat_token (COLUMN_SEP (build_op buf)),		Keep, Set Inline)
			| Tok_row_end buf		-> (Cat_token (ROW_END (build_op buf)),			Keep, Set Block)
			| Tok_plain x			-> (Cat_token (PLAIN x),				Keep, Set Inline)
			| Tok_entity x			-> (Cat_token (ENTITY x),				Keep, Set Inline)
			| Tok_space			-> (Cat_space,						Keep, Keep)
			| Tok_break			-> (Cat_ignore,						Keep, Set Block) in

		let change_state current history = function
			| Set state	-> state
			| Push state	-> Stack.push current history; state
			| Pop		-> Stack.pop history
			| Keep		-> current in

		let new_env = change_state current_env env_history potential_new_env in

		let new_context = change_state current_context context_history potential_new_context in

		let () = match (category, current_context, new_context) with
			| (Cat_begin _, Block, Inline)	-> ()
			| (_, Block, Inline)		-> self#store (NEW_PARAGRAPH (build_op lexbuf))
			| _				-> () in

		let () = match category with
			| Cat_token tok
			| Cat_begin tok
			| Cat_end tok		-> self#store tok
			| Cat_space		-> if new_context = Inline then self#store (PLAIN " ")
			| Cat_ignore		-> () in

		current_env <- new_env;
		current_context <- new_context
end

