(********************************************************************************)
(*	Lambdoc_rlambtex_tokenizer.ml
	Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Tokenizer for the Lambtex reader.  Because the implementation of the Lambtex
	reader relies on different scanners to be invoked in accordance to the current
	environment, and because the significance of spaces depends on whether we find
	ourselves in a block or inline context, instead of the usual parser/lexer
	combination we actually use a parser/tokenizer/scanner architecture.  The
	tokenizer sits between the parser proper and the various scanners, keeping
	a partial view of the current parsing environment so it can choose the
	appropriate scanner to invoke.
*)

module Hashtbl = BatHashtbl
module String = BatString

module Globalenv = Lambdoc_rlambtex_globalenv
module Parser = Lambdoc_rlambtex_parser
module Scanner = Lambdoc_rlambtex_scanner

open Lexing
open Lambdoc_reader
open Ast
open Extension
open Globalenv
open Parser


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type context_t = Blk | Inl | Tab

type action_t =
	| Hold
	| Set of context_t
	| Push of context_t
	| Pop


(********************************************************************************)
(*	{1 Auxiliary regular expressions}					*)
(********************************************************************************)

let pat_ident = "[a-z][a-z0-9_]*"
let pat_begin = "\\\\begin"
let pat_simple = "\\\\(?<simple>" ^ pat_ident ^ ")"
let pat_primary = "\\{(?<primary>" ^ pat_ident ^ ")\\}"

let pat_order = "(?<order>\\([^)\\[\\]<>\\{\\}]*\\))"
let pat_label = "(?<label>\\[[^\\]\\(\\)<>\\{\\}]*\\])"
let pat_style = "(?<style><[^>\\[\\]\\(\\)\\{\\}]*>)"
let pat_optional = "(" ^ pat_order ^ "|" ^ pat_style ^ "|" ^ pat_label ^")*"

let begin_rex = Pcre.regexp ("^" ^ pat_begin ^ pat_optional ^ pat_primary ^ "$")
let simple_rex = Pcre.regexp ("^" ^ pat_simple ^ pat_optional ^ "$")


(********************************************************************************)
(*	{1 Auxiliary functions}							*)
(********************************************************************************)

(**	Processes raw parameters.  Basically it determines whether the parameter
	actually exists, and if so, removes the leading and trailing marker.
*)
let get_param rex name subs =
	try
		let res = Pcre.get_named_substring rex name subs in
		Some (String.slice ~first:1 ~last:(-1) res)	(* safe because 'res' is Latin1 *)
	with
		_ -> None


(**	Builds a fully-featured {!Lambdoc_reader.Ast.command_t}.
*)
let build_command tag rex subs position =
	{
	comm_tag = Some tag;
	comm_label = get_param rex "label" subs;
	comm_order = get_param rex "order" subs;
	comm_style = get_param rex "style" subs;
	comm_linenum = position.pos_lnum;
	}


(**	Builds a {!Lambdoc_reader.Ast.command_t} from an operator.
	Only the line number field is actually set.
*)
let build_op position =
	{
	comm_tag = None;
	comm_label = None;
	comm_order = None;
	comm_style = None;
	comm_linenum = position.pos_lnum;
	}


(**	Does the given block syntax demand the use of an environment?
*)
let is_env (_, syntax) = match syntax with
	| Syn_lit
	| Syn_frag -> true
	| Syn_empty
	| Syn_seq
	| Syn_raw _
	| Syn_raw_raw _
	| Syn_raw_seq _
	| Syn_raw_seqopt _ -> false


(**	Issues the begin tag of an environment command.
*)
let issue_begin_command ~env_block_extdefs raw_comm position =
	let subs = Pcre.exec ~rex:begin_rex raw_comm in
	let primary = Pcre.get_named_substring begin_rex "primary" subs in
	let command = build_command primary begin_rex subs position in
	let first_token =
		if String.starts_with primary "mathtex"
		then BEGIN_MATHTEX_BLK primary
		else if String.starts_with primary "mathml"
		then BEGIN_MATHML_BLK primary
		else if String.starts_with primary "verbatim"
		then BEGIN_VERBATIM primary
		else if String.starts_with primary "pre"
		then BEGIN_VERBATIM primary
		else if String.starts_with primary "source"
		then BEGIN_SOURCE primary
		else match primary with
			| "abstract"	-> BEGIN_ABSTRACT primary
			| "itemize"
			| "itemise"
			| "ul"		-> BEGIN_ITEMIZE primary
			| "enumerate"
			| "ol"		-> BEGIN_ENUMERATE primary
			| "description"
			| "dl"		-> BEGIN_DESCRIPTION primary
			| "qanda"	-> BEGIN_QANDA primary
			| "verse"	-> BEGIN_VERSE primary
			| "quote"	-> BEGIN_QUOTE primary
			| "tabular"	-> BEGIN_TABULAR primary
			| "subpage"	-> BEGIN_SUBPAGE primary
			| "pull"	-> BEGIN_PULLQUOTE primary
			| "equation"	-> BEGIN_EQUATION primary
			| "printout"	-> BEGIN_PRINTOUT primary
			| "table"	-> BEGIN_TABLE primary
			| "figure"	-> BEGIN_FIGURE primary
			| "bib"		-> BEGIN_BIB primary
			| "note"	-> BEGIN_NOTE primary
			| x ->
				try
					let (tag, syntax) = List.find (fun (tag, _) -> String.starts_with x tag) env_block_extdefs in
					match syntax with
						| Syn_lit  -> BEGIN_BLKPAT_LIT tag
						| Syn_frag -> BEGIN_BLKPAT_FRAG tag
						| _	   -> assert false
				with
					Not_found -> BEGIN_CUSTOM x
	and second_token = BEGIN_DUMMY command in
	(Set Blk, [first_token; second_token])


(**	Issues a simple command.
*)
let issue_simple_command ~inline_extdefs ~sim_block_extdefs raw_comm position =
	let subs = Pcre.exec ~rex:simple_rex raw_comm in
	let simple = Pcre.get_named_substring simple_rex "simple" subs in
	let command = build_command ("\\" ^ simple) simple_rex subs position in
	let (context, token) = match simple with
		| "br"				-> (Inl, LINEBREAK command)
		| "glyph"			-> (Inl, GLYPH command)
		| "bold" | "strong" | "b"	-> (Inl, BOLD command)
		| "emph" | "em" | "i"		-> (Inl, EMPH command)
		| "code" | "tt"			-> (Inl, CODE command)
		| "caps"			-> (Inl, CAPS command)
		| "ins"				-> (Inl, INS command)
		| "del"				-> (Inl, DEL command)
		| "sup"				-> (Inl, SUP command)
		| "sub"				-> (Inl, SUB command)
		| "mbox"			-> (Inl, MBOX command)
		| "span"			-> (Inl, SPAN command)
		| "link" | "a"			-> (Inl, LINK command)
		| "see"				-> (Inl, SEE command)
		| "cite"			-> (Inl, CITE command)
		| "dref"			-> (Inl, DREF command)
		| "sref"			-> (Inl, SREF command)
		| "mref"			-> (Inl, MREF command)
		| "paragraph" | "p"		-> (Blk, PARAGRAPH command)
		| "picture"			-> (Blk, PICTURE command)
		| "part"			-> (Blk, PART command)
		| "appendix"			-> (Blk, APPENDIX command)
		| "h1" | "section"		-> (Blk, SECTION (command, 1))
		| "h2" | "subsection"		-> (Blk, SECTION (command, 2))
		| "h3" | "subsubsection"	-> (Blk, SECTION (command, 3))
		| "h4"				-> (Blk, SECTION (command, 4))
		| "h5"				-> (Blk, SECTION (command, 5))
		| "h6"				-> (Blk, SECTION (command, 6))
		| "bibliography"		-> (Blk, BIBLIOGRAPHY command)
		| "notes"			-> (Blk, NOTES command)
		| "toc"				-> (Blk, TOC command)
		| "title"			-> (Blk, TITLE (command, 1))
		| "subtitle"			-> (Blk, TITLE (command, 2))
		| "rule" | "hr"			-> (Blk, RULE command)
		| "newmacro"			-> (Blk, MACRODEF command)
		| "newboxout"			-> (Blk, BOXOUTDEF command)
		| "newtheorem"			-> (Blk, THEOREMDEF command)
		| "item" | "li"			-> (Blk, ITEM command)
		| "question"			-> (Blk, QUESTION command)
		| "rquestion"			-> (Blk, RQUESTION command)
		| "answer"			-> (Blk, ANSWER command)
		| "ranswer"			-> (Blk, RANSWER command)
		| "head"			-> (Blk, THEAD command)
		| "foot"			-> (Blk, TFOOT command)
		| "body"			-> (Blk, TBODY command)
		| "who"				-> (Blk, BIB_AUTHOR command)
		| "what"			-> (Blk, BIB_TITLE command)
		| "where"			-> (Blk, BIB_RESOURCE command)
		| "arg"				-> (Inl, MACROARG command)
		| x ->
			let maybe_assoc key xs = try Some (List.assoc key xs) with Not_found -> None in
			match maybe_assoc x inline_extdefs with
				| Some Syn_empty	-> (Inl, INLPAT_EMPTY (command, x))
				| Some Syn_seq		-> (Inl, INLPAT_SEQ (command, x))
				| Some Syn_raw _	-> (Inl, INLPAT_RAW (command, x))
				| Some Syn_raw_raw _	-> (Inl, INLPAT_RAW_RAW (command, x))
				| Some Syn_raw_seq _	-> (Inl, INLPAT_RAW_SEQ (command, x))
				| Some Syn_raw_seqopt _	-> (Inl, INLPAT_RAW_SEQOPT (command, x))
				| Some _		-> assert false
				| None -> match maybe_assoc x sim_block_extdefs with
					| Some Syn_empty	-> (Blk, BLKPAT_EMPTY (command, x))
					| Some Syn_seq		-> (Blk, BLKPAT_SEQ (command, x))
					| Some Syn_raw _	-> (Blk, BLKPAT_RAW (command, x))
					| Some Syn_raw_raw _	-> (Blk, BLKPAT_RAW_RAW (command, x))
					| _			-> (Inl, MACROCALL (command, x))
	in (Set context, [token])


(********************************************************************************)
(**	{1 Tokenizer class}							*)
(********************************************************************************)

class tokenizer ~linenum_offset ~(inline_extdefs:Extension.extdef_t list) ~(block_extdefs:Extension.extdef_t list) =
	let (env_block_extdefs, sim_block_extdefs) = List.partition is_env block_extdefs in
	let issue_begin_command = issue_begin_command ~env_block_extdefs in
	let issue_simple_command = issue_simple_command ~inline_extdefs ~sim_block_extdefs in
object (self)

	val mutable context = Blk
	val mutable history = []
	val mutable productions = []


	(**	The current position of the scanner.  This is a value
		of type [Lexing.position], as required by Menhir.
	*)
	val mutable position =
		{
		pos_fname = "";
		pos_lnum = linenum_offset + 1;
		pos_bol = 0;
		pos_cnum = 0;
		}


	(**	Recomputes the current position of the scanner.
	*)
	method private update_position num_newlines =
		position <- {position with pos_lnum = position.pos_lnum + num_newlines}


	(**	Updates the current context according to the context action.
	*)
	method private update_context action =
		let (new_context, new_history) = match action with
			| Hold	   -> (context, history)
			| Set ctx  -> (ctx, history)
			| Push ctx -> (ctx, context :: history)
			| Pop	   -> match history with hd::tl -> (hd, tl) | [] -> (context, history)
		in
			context <- new_context;
			history <- new_history


	(**	Stores a new token into the production queue.
	*)
	method private store token =
		productions <- match (productions, token) with
			| ([PLAIN (op1, txt1)], PLAIN (op2, txt2))	-> [PLAIN (op1, BatText.append txt1 txt2)]
			| ([RAW txt1], RAW txt2)			-> [RAW (BatText.append txt1  txt2)]
			| _						-> productions @ [token]


	(**	Produce new tokens.
	*)
	method private produce lexbuf =
		let scanner = match Globalenv.get_scanner () with
			| Scanner_general	-> Scanner.general_scanner
			| Scanner_raw		-> Scanner.raw_scanner
			| Scanner_mathtex_inl	-> Scanner.mathtex_inl_scanner
			| Scanner_mathml_inl	-> Scanner.mathml_inl_scanner
			| Scanner_tabular	-> Scanner.tabular_scanner
			| Scanner_literal term	-> Scanner.literal_scanner term in
		let (num_newlines, raw_token) = scanner lexbuf in
		let op = build_op self#position in
		let (action, tokens) = match raw_token with
			| `Tok_simple_comm comm		-> issue_simple_command comm self#position
			| `Tok_env_begin comm		-> issue_begin_command comm self#position
			| `Tok_env_end comm		-> (Set Blk, [END_DUMMY comm; END_BLOCK])
			| `Tok_begin			-> (Push Inl, [BEGIN; OPEN_DUMMY])
			| `Tok_end			-> (Pop, [CLOSE_DUMMY; END])
			| `Tok_begin_mathtex_inl	-> (Set Inl, [BEGIN_MATHTEX_INL op; OPEN_DUMMY])
			| `Tok_end_mathtex_inl		-> (Hold, [CLOSE_DUMMY; END_MATHTEX_INL op])
			| `Tok_begin_mathml_inl		-> (Set Inl, [BEGIN_MATHML_INL op; OPEN_DUMMY])
			| `Tok_end_mathml_inl		-> (Hold, [CLOSE_DUMMY; END_MATHML_INL op])
			| `Tok_cell_mark		-> (Set Tab, [CELL_MARK op])
			| `Tok_row_end			-> (Set Tab, [ROW_END op])
			| `Tok_eof			-> (Hold, [EOF])
			| `Tok_parbreak			-> (Set Blk, [])
			| `Tok_space when context = Inl	-> (Hold, [PLAIN (op, BatText.of_string " ")])
			| `Tok_space			-> (Hold, [])
			| `Tok_raw txt			-> (Hold, [RAW (BatText.of_string txt)])
			| `Tok_plain txt		-> (Set Inl, [PLAIN (op, BatText.of_string txt)])
			| `Tok_entity ent		-> (Set Inl, [ENTITY (op, ent)]) in
		let tokens = match (context, action) with
			| (Blk, Set Inl) -> (NEW_PAR op) :: tokens
			| _		 -> tokens
		in
			self#update_position num_newlines;
			self#update_context action;
			List.iter self#store tokens


	(**	Returns a new token.
	*)
	method consume lexbuf = match productions with
		| []
		| [PLAIN (_, _)]
		| [RAW _]	-> self#produce lexbuf; self#consume lexbuf
		| hd :: tl	-> productions <- tl; hd


	(**	Returns the current scanner position.
	*)
	method position = position
end

