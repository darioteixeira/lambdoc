(********************************************************************************)
(*	Implementation file for Lambtex_tokenizer.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed nestable the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Tokenizer for the Lambtex reader.  Because the implementation of the
	Lambtex reader relies on different scanners to be invoked in accordance
	to the language construct currently being parsed, the tokenizer sits
	between the parser proper and the various scanners, keeping a partial
	view of the current parsing context/environment so it can choose the
	appropriate scanner to invoke.
*)

open Lexing
open ExtString
open Lambdoc_reader
open Ast
open Parser
open Scanner


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
	| Raw
	| Mathtex_inl
	| Mathml_inl
	| Mathtex_blk of string
	| Mathml_blk of string
	| Verbatim of string
	| Code of string


(**	Declaration of the two scanning contexts.  A [Blk] (block) context
	means that whitespace is ignored, while in an [Inl] (inline) context
	whitespace is relevant.
*)
type context_t =
	| Blk
	| Inl


(**	Actions for the automaton that changes the tokenizer state.  The
	actions are as follows:
	{ul	{li [Set_con]: Changes the current context;}
		{li [Push_con]: Pushes the current context into the context history;}
		{li [Pop_con]: Pops a context from the context history, using it to set the current context;}
		{li [Push_env]: Pushes a new environment into the environment history;}
		{li [Pop_env]: Pops the last environment from the environment history;}
		{li [Store]: Stores a list of future environments;}
		{li [Fetch]: Fetches the first environment from storage and pushes into the environment history.}}
*)
type action_t =
	| Set_con of context_t
	| Push_con
	| Pop_con
	| Push_env of environment_t
	| Pop_env
	| Store of environment_t list
	| Fetch


(********************************************************************************)
(**	{2 Tag-building functions}						*)
(********************************************************************************)

(**	This function returns the tag information corresponding to the string form
	of the supplied simple tag.  A simple tag is one whose begin is signaled by
	its own escaped name.  In similarity to {!get_env_tag}, this function returns
	a pair consisting of the parser token and a list of actions for the automaton.
*)
let get_simple_tag tag params =
	let (token, context, actions) = match tag with
		| "bold"		-> (BOLD params,		Inl,	[Store [Inline]])
		| "emph"		-> (EMPH params,		Inl,	[Store [Inline]])
		| "mono"		-> (MONO params,		Inl,	[Store [Inline]])
		| "caps"		-> (CAPS params,		Inl,	[Store [Inline]])
		| "thru"		-> (THRU params,		Inl,	[Store [Inline]])
		| "sup"			-> (SUP params,			Inl,	[Store [Inline]])
		| "sub"			-> (SUB params,			Inl,	[Store [Inline]])
		| "mbox"		-> (MBOX params,		Inl,	[Store [Inline]])

		| "link"		-> (LINK params,		Inl,	[Store [Raw; Inline]])
		| "see"			-> (SEE params,			Inl,	[Store [Raw]])
		| "cite"		-> (CITE params,		Inl,	[Store [Raw]])
		| "ref"			-> (REF params,			Inl,	[Store [Raw]])
		| "sref"		-> (SREF params,		Inl,	[Store [Raw]])
		| "mref"		-> (MREF params,		Inl,	[Store [Raw; Inline]])

		| "part"		-> (PART params, 		Blk,	[Store [Inline]])
		| "appendix"		-> (APPENDIX params,		Blk,	[])
		| "section"		-> (SECTION params,		Blk,	[Store [Inline]])
		| "subsection"		-> (SUBSECTION params,		Blk,	[Store [Inline]])
		| "subsubsection"	-> (SUBSUBSECTION params,	Blk,	[Store [Inline]])
		| "bibliography"	-> (BIBLIOGRAPHY params,	Blk,	[])
		| "notes"		-> (NOTES params,		Blk,	[])
		| "toc"			-> (TOC params,			Blk,	[])
		| "title"		-> (TITLE params, 		Blk,	[Store [Inline]])
		| "subtitle"		-> (SUBTITLE params, 		Blk,	[Store [Inline]])
		| "rule"		-> (RULE params,		Blk,	[])

		| "item"		-> (ITEM params,		Blk,	[])
		| "describe"		-> (DESCRIBE params,		Blk,	[Store [Inline]])
		| "bitmap"		-> (BITMAP params,		Blk,	[Store [Raw; Raw]])
		| "caption"		-> (CAPTION params,		Blk,	[Store [Inline]])
		| "head"		-> (HEAD params,		Blk,	[])
		| "foot"		-> (FOOT params,		Blk,	[])
		| "body"		-> (BODY params,		Blk,	[])
		| "who"			-> (BIB_AUTHOR params,		Blk,	[Store [Inline]])
		| "what"		-> (BIB_TITLE params,		Blk,	[Store [Inline]])
		| "where"		-> (BIB_RESOURCE params,	Blk,	[Store [Inline]])
		| other			-> raise (Unknown_simple_command other)
	in (Some token, (Set_con context) :: actions)


(**	This function returns the tag information corresponding to the string
	form of the supplied environment tag.  An environment tag is one that
	begins with a \begin declaration and ends with a corresponding \end.
	The information returned is a pair consisting of the parser token,
	and a list of actions for the automaton.

*)
let get_env_tag tag params is_begin =
	let (token_begin, token_end, actions_begin, actions_end) =

		(* First check if it matches any of the literal environment prefixes. *)

		if String.starts_with tag "mathtex"
		then (BEGIN_MATHTEX_BLK params, END_MATHTEX_BLK params, [Push_env (Mathtex_blk tag)], [Pop_env])
		else if String.starts_with tag "mathml"
		then (BEGIN_MATHML_BLK params, END_MATHML_BLK params, [Push_env (Mathml_blk tag)], [Pop_env])
		else if String.starts_with tag "verbatim"
		then (BEGIN_VERBATIM params, END_VERBATIM params, [Push_env (Verbatim tag)], [Pop_env])
		else if String.starts_with tag "code"
		then (BEGIN_CODE params, END_CODE params, [Push_env (Code tag)], [Pop_env])

		(* If not literal, then test the other environments. *)

		else match tag with
			| "itemize"	-> (BEGIN_ITEMIZE params,	END_ITEMIZE params,		[],			[])
			| "enumerate"	-> (BEGIN_ENUMERATE params,	END_ENUMERATE params,		[],			[])
			| "description"	-> (BEGIN_DESCRIPTION params,	END_DESCRIPTION params,		[],			[])
			| "quote"	-> (BEGIN_QUOTE params,		END_QUOTE params,		[],			[])
			| "callout"	-> (BEGIN_CALLOUT params,	END_CALLOUT params,		[Store [Inline]],	[])
			| "tabular"	-> (BEGIN_TABULAR params,	END_TABULAR params,		[Push_env Tabular],	[Pop_env])
			| "subpage"	-> (BEGIN_SUBPAGE params,	END_SUBPAGE params,		[],			[])
			| "equation"	-> (BEGIN_EQUATION params,	END_EQUATION params,		[],			[])
			| "printout"	-> (BEGIN_PRINTOUT params,	END_PRINTOUT params,		[],			[])
			| "table"	-> (BEGIN_TABLE params,		END_TABLE params,		[],			[])
			| "figure"	-> (BEGIN_FIGURE params,	END_FIGURE params,		[],			[])
			| "abstract"	-> (BEGIN_ABSTRACT params,	END_ABSTRACT params,		[],			[])
			| "bib"		-> (BEGIN_BIB params,		END_BIB params,			[],			[])
			| "note"	-> (BEGIN_NOTE params,		END_NOTE params,		[],			[])
			| other		-> raise (Unknown_env_command other)

	in if is_begin
	then (Some (token_begin), (Set_con Blk) :: actions_begin)
	else (Some (token_end), (Set_con Blk) :: actions_end)


(********************************************************************************)
(**	{2 Functions to process commands and parameters}			*)
(********************************************************************************)

(**	Declaration of various regular expressions.
*)

let pat_env = "\\\\(?<env>(begin)|(end))"
let pat_command = "\\\\(?<command>\\w+)"
let pat_primary = "\\{(?<primary>\\w+[\\w\\d_]*)\\}"
let pat_secondary = "(?<secondary>\\{\\w*\\})?"

let pat_order = "(?<order>\\([\\w\\d\\.]*\\))"
let pat_label = "(?<label>\\[[\\w\\d\\-:_]*\\])"
let pat_extra = "(?<extra><[\\w\\d=,!]*>)"
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


(**	Builds a fully-featured {!Document_ast.Ast.command_t}.
*)
let build_command lexbuf tag rex subs =
	{
	comm_tag = Some tag;
	comm_label = get_param rex "label" subs;
	comm_order = get_param rex "order" subs;
	comm_extra = get_param rex "extra" subs;
	comm_secondary = get_param rex "secondary" subs;
	comm_linenum = lexbuf.lex_curr_p.pos_lnum;
	}


(**	Builds a {!Document_ast.Ast.command_t} from an operator.
	Only the line number field is set.
*)
let build_op lexbuf =
	{
	comm_tag = None;
	comm_label = None;
	comm_order = None;
	comm_extra = None;
	comm_secondary = None;
	comm_linenum = lexbuf.lex_curr_p.pos_lnum;
	}


(**	Issues an environment command.
*)
let issue_env_command =
	let rex = Pcre.regexp ("^" ^ pat_env ^ pat_optional ^ pat_primary ^ pat_secondary ^ "$")
	in fun lexbuf ->
		let subs = Pcre.exec ~rex (Lexing.lexeme lexbuf) in
		let command = Pcre.get_named_substring rex "env" subs
		and primary = Pcre.get_named_substring rex "primary" subs in
		let params = build_command lexbuf primary rex subs in
		get_env_tag primary params (command = "begin")


(**	Issues a simple command.
*)
let issue_simple_command =
	let rex = Pcre.regexp ("^" ^ pat_command ^ pat_optional ^ "$")
	in fun lexbuf ->
		let subs = Pcre.exec ~rex (Lexing.lexeme lexbuf) in
		let command = Pcre.get_named_substring rex "command" subs in
		let params = build_command lexbuf command rex subs in
		get_simple_tag command params


(********************************************************************************)
(**	{2 Tokenizer class}							*)
(********************************************************************************)

class tokenizer =
object (self)

	(**	The state of the tokenizer: it consists of the current context,
		the context history, the environment history, the storage for
		future environments, and the production queue.
	*)

	val mutable productions = []
	val mutable context = Blk
	val context_history = Stack.create ()
	val env_history = Stack.create ()
	val env_storage = Queue.create ()


	(**	Returns the element at the top of the environment history.
	*)
	method get_env () =
		try
			Some (Stack.top env_history)
		with
			Stack.Empty -> None


	(**	Performs the given {!action_t}, changing the state of the automaton.
	*)
	method private perform_action = function
		| Set_con con		-> context <- con
		| Push_con		-> Stack.push context context_history
		| Pop_con		-> context <- (try Stack.pop context_history with Stack.Empty -> context)
		| Push_env env		-> Stack.push env env_history
		| Pop_env		-> (try ignore (Stack.pop env_history) with Stack.Empty -> ())
		| Store envs		-> List.iter (fun x -> Queue.add x env_storage) envs
		| Fetch			-> try Stack.push (Queue.take env_storage) env_history with Queue.Empty -> ()


	(**	Consumer method.  Given a [lexbuf], consumes a token from the
		lexer stream and returns it to the caller.
	*)
	method consume lexbuf = match productions with
		| []
		| [PLAIN (_, _)]
		| [RAW _]		-> self#produce lexbuf; self#consume lexbuf
		| hd :: tl		-> productions <- tl; hd


	(**	Stores a new token into the production queue.
	*)
	method store token =
		productions <- match (productions, token) with
			| ([PLAIN (op1, txt1)], PLAIN (op2, txt2))	-> [PLAIN (op1, txt1 ^ txt2)]
			| ([RAW txt1], RAW txt2)			-> [RAW (txt1 ^ txt2)]
			| _						-> productions @ [token]


	(**	Private method that actually does all the work of invoking the
		scanner to produce a new token.
	*)
	method private produce lexbuf =

		let scanner = match self#get_env () with
			| Some Inline		-> Scanner.general_scanner
			| Some Tabular		-> Scanner.tabular_scanner
			| Some Verbatim term	-> Scanner.literal_scanner term
			| Some Code term	-> Scanner.literal_scanner term
			| Some Raw		-> Scanner.raw_scanner
			| Some Mathtex_inl	-> Scanner.mathtex_inl_scanner
			| Some Mathml_inl	-> Scanner.mathml_inl_scanner
			| Some Mathtex_blk term	-> Scanner.literal_scanner term
			| Some Mathml_blk term	-> Scanner.literal_scanner term
			| None			-> Scanner.general_scanner in

		let (maybe_token, actions) = match scanner lexbuf with
			| `Tok_simple_comm buf			-> issue_simple_command buf
			| `Tok_env_begin buf			-> issue_env_command buf
			| `Tok_env_end buf			-> issue_env_command buf
			| `Tok_begin				-> (Some BEGIN,					[Fetch; Push_con])
			| `Tok_end				-> (Some END,					[Pop_env; Pop_con])
			| `Tok_begin_mathtex_inl buf		-> (Some (BEGIN_MATHTEX_INL (build_op buf)),	[Set_con Inl; Push_con; Push_env Mathtex_inl])
			| `Tok_end_mathtex_inl buf		-> (Some (END_MATHTEX_INL (build_op buf)),	[Pop_env; Pop_con])
			| `Tok_begin_mathml_inl buf		-> (Some (BEGIN_MATHML_INL (build_op buf)),	[Set_con Inl; Push_con; Push_env Mathml_inl])
			| `Tok_end_mathml_inl buf		-> (Some (END_MATHML_INL (build_op buf)),	[Pop_env; Pop_con])
			| `Tok_column_sep buf			-> (Some (COLUMN_SEP (build_op buf)),		[Set_con Blk])
			| `Tok_row_end buf			-> (Some (ROW_END (build_op buf)),		[Set_con Blk])
			| `Tok_eof				-> (Some EOF,					[])
			| `Tok_break				-> (None,					[Set_con Blk])
			| `Tok_space buf when context = Blk	-> (None,					[])
			| `Tok_space buf when context = Inl	-> (Some (PLAIN (build_op buf, " ")),		[])
			| `Tok_raw txt				-> (Some (RAW txt),				[])
			| `Tok_plain (buf, txt)			-> (Some (PLAIN (build_op buf, txt)),		[Set_con Inl])
			| `Tok_entity (buf, txt)		-> (Some (ENTITY (build_op buf, txt)),		[Set_con Inl])
			| _					-> failwith "Unexpected scanner token" in

		let old_context = context in

		let () =
			List.iter self#perform_action actions;
			match (old_context, context, self#get_env ()) with
				| (Blk, Inl, None)	-> self#store (NEW_PAR (build_op lexbuf))
				| _			-> ()

		in match maybe_token with
			| Some token	-> self#store token
			| None		-> ()
end

