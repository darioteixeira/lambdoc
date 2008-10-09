(********************************************************************************)
(*	Tokenizer for the Lambtex reader.  The tokenizer sits between the parser
	proper and the various scanners.  Because the implementation of the Lambtex
	reader relies on different scanners to be invoked in accordance to the
	language construct currently being parsed, the tokenizer sits between
	the parser proper and the various scanners, keeping a partial view of
	the current parsing context/environment so it can choose the appropriate
	scanner to invoke.

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

(**	Declaration of the various kinds of scanning environments.  Most of
	these are triggered by begin/end commands, except for the inline math
	environments and inline simple commands.
*)
type environment_t =
	| Inline
	| Tabular
	| Verbatim
	| Code
	| Raw
	| Mathtex_inl
	| Mathml_inl
	| Mathtex_blk
	| Mathml_blk


(**	Declaration of the two scanning contexts.  A [Blk] (block) context
	means that whitespace is ignored, while in an [Inl] (inline) context
	whitespace is relevant.
*)
type context_t =
	| Blk
	| Inl


(**	Actions for the automaton that changes the tokenizer state.  The
	actions are as follows:
	{ul	{li [Context]: Changes the current {!context_t}.};
		{li [Push]: Pushes a new {!environment_t} into the stack;
		{li [Pop]: Pops the top {!environment_t} from the stack;
		{li [Store]: Stores a list of future {!environment_t};
		{li [Fetch]: Fetches an {!environment_t} from storage and
		pushes into the stack.}}
*)
type action_t =
	| Context of context_t
	| Push of environment_t
	| Pop
	| Store of environment_t list
	| Fetch


(********************************************************************************)
(**	{2 Tag-building functions}						*)
(********************************************************************************)

(**	This function returns the tag information corresponding to the string
	form of the supplied environment tag.  An environment tag is one that
	begins with a \begin declaration and ends with a corresponding \end.
	The information returned is a pair consisting of the parser token,
	and a list of actions for the automaton.

*)
let get_env_tag params is_begin =
	let (token_begin, token_end, actions_begin, actions_end) = match params.comm_tag with
		| "abstract"	-> (BEGIN_ABSTRACT params,	END_ABSTRACT params,		[],			[])
		| "itemize"	-> (BEGIN_ITEMIZE params,	END_ITEMIZE params,		[],			[])
		| "enumerate"	-> (BEGIN_ENUMERATE params,	END_ENUMERATE params,		[],			[])
		| "quote"	-> (BEGIN_QUOTE params,		END_QUOTE params,		[],			[])
		| "tex"		-> (BEGIN_MATHTEX_BLK params,	END_MATHTEX_BLK params,		[Push Mathtex_blk],	[Pop])
		| "mathml"	-> (BEGIN_MATHML_BLK params,	END_MATHML_BLK params,		[Push Mathml_blk],	[Pop])
		| "code"	-> (BEGIN_CODE params,		END_CODE params,		[Push Code],		[Pop])
		| "verbatim"	-> (BEGIN_VERBATIM params,	END_VERBATIM params,		[Push Verbatim],	[Pop])
		| "tabular"	-> (BEGIN_TABULAR params,	END_TABULAR params,		[Push Tabular],		[Pop])
		| "subpage"	-> (BEGIN_SUBPAGE params,	END_SUBPAGE params,		[],			[])
		| "equation"	-> (BEGIN_EQUATION params,	END_EQUATION params,		[],			[])
		| "algorithm"	-> (BEGIN_ALGORITHM params,	END_ALGORITHM params,		[],			[])
		| "table"	-> (BEGIN_TABLE params,		END_TABLE params,		[],			[])
		| "figure"	-> (BEGIN_FIGURE params,	END_FIGURE params,		[],			[])
		| "bib"		-> (BEGIN_BIB params,		END_BIB params,			[],			[])
		| other		-> raise (Unknown_env_command other)
	in if is_begin
	then (Some (token_begin), (Context Blk) :: actions_begin)
	else (Some (token_end), (Context Blk) :: actions_end)


(**	This function returns the tag information corresponding to the string form
	of the supplied simple tag.  A simple tag is one whose begin is signaled by
	its own escaped name.  In similarity to {!get_env_tag}, this function returns
	a pair consisting of the parser token and a list of actions for the automaton.
*)
let get_simple_tag params =
	let (token, context, actions) = match params.comm_tag with
		| "bold"		-> (BOLD params,		Inl,		[Store [Inline]])
		| "emph"		-> (EMPH params,		Inl,		[Store [Inline]])
		| "mono"		-> (MONO params,		Inl,		[Store [Inline]])
		| "caps"		-> (CAPS params,		Inl,		[Store [Inline]])
		| "thru"		-> (THRU params,		Inl,		[Store [Inline]])
		| "sup"			-> (SUP params,			Inl,		[Store [Inline]])
		| "sub"			-> (SUB params,			Inl,		[Store [Inline]])
		| "box"			-> (BOX params,			Inl,		[Store [Inline]])

		| "link"		-> (LINK params,		Inl,		[Store [Raw; Inline]])
		| "see"			-> (SEE params,			Inl,		[Store [Raw]])
		| "cite"		-> (CITE params,		Inl,		[Store [Raw]])
		| "ref"			-> (REF params,			Inl,		[Store [Raw]])
		| "sref"		-> (SREF params,		Inl,		[Store [Raw]])
		| "mref"		-> (MREF params,		Inl,		[Store [Raw; Inline]])

		| "section"		-> (SECTION params,		Blk,		[Store [Inline]])
		| "subsection"		-> (SUBSECTION params,		Blk,		[Store [Inline]])
		| "subsubsection"	-> (SUBSUBSECTION params,	Blk,		[Store [Inline]])
		| "toc"			-> (TOC params,			Blk,		[])
		| "bibliography"	-> (BIBLIOGRAPHY params,	Blk,		[])
		| "notes"		-> (NOTES params,		Blk,		[])

		| "title"		-> (TITLE params, 		Blk,		[Store [Inline]])
		| "rule"		-> (RULE params,		Blk,		[])
		| "appendix"		-> (APPENDIX params,		Blk,		[])
		| "item"		-> (NEW_ITEM params,		Blk,		[])
		| "image"		-> (IMAGE params,		Blk,		[Store [Raw]])
		| "caption"		-> (CAPTION params,		Blk,		[Store [Inline]])
		| "head"		-> (HEAD params,		Blk,		[])
		| "foot"		-> (FOOT params,		Blk,		[])
		| "body"		-> (BODY params,		Blk,		[])
		| "btitle"		-> (BIB_TITLE params,		Blk,		[Store [Inline]])
		| "bauthor"		-> (BIB_AUTHOR params,		Blk,		[Store [Inline]])
		| "bresource"		-> (BIB_RESOURCE params,	Blk,		[Store [Inline]])
		| "note"		-> (NOTE params,		Blk,		[Store [Inline]])
		| other			-> raise (Unknown_simple_command other)
	in (Some token, (Context context) :: actions)


(********************************************************************************)
(**	{2 Functions to process commands and parameters}			*)
(********************************************************************************)

(**	Declaration of various regular expressions.
*)

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


(**	Builds a {!Document_ast.Ast.command_t}.
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


(**	Builds a {!Document_ast.Ast.operator_t}.
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

	(**	The state of the tokenizer: it consists of the current context
		(a value of type {!context_t]}, the environment history (a list
		of {!environment_t}), the storage for future environments (a
		{!environment_t Queue.t}), and the production queue.
	*)

	val mutable context = Blk
	val mutable history = []
	val storage = Queue.create ()
	val productions = Queue.create ()


	(**	Consumer method.  Given a [lexbuf], consumes a token from the
		lexer stream and returns it to the caller.
	*)
	method consume lexbuf =
		try
			Queue.take productions
		with
			Queue.Empty ->
				self#produce lexbuf;
				self#consume lexbuf

	(**	Performs the given {!action_t}, changing the state of the automaton.
	*)
	method private perform_action = function
		| Context con		-> context <- x
		| Push scanner		-> Stack.push scanner history
		| Pop			-> try Stack.pop history with Stack.Empty -> ()
		| Store scanners	-> List.iter (Queue.add storage) scanners
		| Fetch			-> try history <- (Queue.take storage) :: history with Queue.Empty -> ()


	(**	Private method that actually does all the work of invoking the
		scanner to produce a new token.
	*)
	method private produce lexbuf =

		let scanner = match (context, history) with
			| (_, Inline :: _)		-> Lambtex_scanner.inline_scanner
			| (_, Tabular :: _)		-> Lambtex_scanner.tabular_scanner
			| (_, Verbatim :: _)		-> Lambtex_scanner.verbatim_scanner
			| (_, Code :: _)		-> Lambtex_scanner.code_scanner
			| (_, Raw :: _)			-> Lambtex_scanner.raw_scanner
			| (_, Mathtex_inl :: _)		-> Lambtex_scanner.mathtex_inl_scanner
			| (_, Mathml_inl :: _)		-> Lambtex_scanner.mathml_inl_scanner
			| (_, Mathtex_blk :: _)		-> Lambtex_scanner.mathtex_blk_scanner
			| (_, Mathml_blk :: _)		-> Lambtex_scanner.mathml_blk_scanner
			| (Inl, _)			-> Lambtex_scanner.inline_scanner
			| (Blk, _)			-> Lambtex_scanner.block_scanner in

		let (maybe_token, actions) = match scanner lexbuf with
			| `Tok_simple_comm buf		-> issue_simple_command buf
			| `Tok_begin_comm buf		-> issue_env_command buf
			| `Tok_end_comm buf		-> issue_env_command buf
			| `Tok_begin buf		-> (Some (BEGIN (build_op buf)),		[Fetch])
			| `Tok_end buf			-> (Some (END (build_op buf)),			[Pop])
			| `Tok_begin_mathtex_inl buf	-> (Some (BEGIN_MATHTEX_INL (build_op buf)),	[Context Inl; Push Mathtex_inl])
			| `Tok_end_mathtex_inl buf	-> (Some (END_MATHTEX_INL (build_op buf)),	[Pop])
			| `Tok_begin_mathml_inl buf	-> (Some (BEGIN_MATHML_INL (build_op buf)),	[Context Inl; Push Mathml_inl])
			| `Tok_end_mathml_inl buf	-> (Some (END_MATHML_INL (build_op buf)),	[Pop])
			| `Tok_column_sep buf		-> (Some (COLUMN_SEP (build_op buf)),		[Context Blk])
			| `Tok_row_end buf		-> (Some (ROW_END (build_op buf)),		[Context Blk])
			| `Tok_break			-> (None,					[Context Blk])
			| `Tok_eof buf			-> (Some (EOF (build_op buf)),			[])
			| `Tok_raw (buf, x)		-> (Some (RAW (build_op buf, x)),		[])
			| `Tok_plain (buf, x)		-> (Some (PLAIN (build_op buf, x)),		[Context Inl])
			| `Tok_entity (buf, x)		-> (Some (ENTITY (build_op buf, x)),		[Context Inl]) in

		let old_context = context in
		List.iter self#perform_action actions;
		match (old_context, context) with
			| (Blk, Inl)	-> Queue.add productions (NEW_PARAGRAPH (build_op lexbuf))
			| _		-> ()

		in match maybe_token with
			| Some token	-> token
			| None		-> self#produce lexbuf
end

