(********************************************************************************)
(**	Scanner for the Lambtex reader.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)


(********************************************************************************)
(**	{2 Auxiliary types and functions}					*)
(********************************************************************************)

{
open Lexing
open ExtString
open Lambtex_parser


(**	The various kinds of tokens output by any of the scanners.
*)
type scanner_token_t =
	| Tok_env_comm of Lexing.lexbuf
	| Tok_simple_comm of Lexing.lexbuf
	| Tok_begin of Lexing.lexbuf
	| Tok_end of Lexing.lexbuf
	| Tok_eof of Lexing.lexbuf
	| Tok_column_sep of Lexing.lexbuf
	| Tok_row_end of Lexing.lexbuf
	| Tok_plain of string
	| Tok_entity of string
	| Tok_space
	| Tok_break


(**	Increments the pos_lnum field, since ocamllex or ulex
	generated lexers do not do this automatically.
*)
let incr_linenum lexbuf =
	let str = Lexing.lexeme lexbuf in
	let rec count_eol this_pos =
	try
		let next_pos = String.index_from str this_pos '\n'
		in 1 + (count_eol (next_pos + 1))
	with
		Not_found -> 0 in
	let num = count_eol 0 in
	let pos = lexbuf.lex_curr_p
	in lexbuf.lex_curr_p <- {pos with pos_lnum = pos.pos_lnum + num;}
}


(********************************************************************************)
(**	{2 List of regular expressions used in scanner}				*)
(********************************************************************************)

let alpha = ['a'-'z' 'A'-'Z']
let deci = ['0'-'9']
let order_char = ['0'-'9' '.']
let label_char = ['a'-'z' 'A'-'Z' '0'-'9' '-' ':' '_']
let extra_char = ['a'-'z' 'A'-'Z' '0'-'9' ',' '!']
let order = '(' order_char* ')'
let label = '[' label_char+ ']'
let extra = '<' extra_char+ '>'
let optional = ( order | label | extra )*
let primary = '{' alpha+ '}'
let secondary = '{' alpha* '}'
let env_command = '\\' ("begin" | "end") optional primary secondary?
let simple_command = '\\' alpha+ optional
let entity = '&' (alpha+ | ('#' deci+))  ';'
let space = [' ' '\t']
let escape = '\\'
let eol = space* '\n'
let break = eol eol+
let begin_marker = '{'
let end_marker = '}'
let column_sep = space* '|' space*
let row_end = space* '$'


(********************************************************************************)
(**	{2 Actual scanners}							*)
(********************************************************************************)

(**	There are five possible scanning contexts in a Lambtex document, each
	demanding special rules from the scanner (though the rules for two of
	these contexts are identical).  We therefore use four different scanner
	functions.  Note that in all scanners, some actions could have been
	simplified if we had used Ocamllex's "as" operator.  We chose not to
	do so because ulex has no equivalent to this operator, and we prefer
	code that is easily adapted to different lexer generators.
*)

(**	The default scanner for documents.  This scanner is used in Inline
	and Block contexts.  Note that the backslash is used as an escape
	character and HTML entities are also recognised.  Also noteworthy
	is that spacing and line breaks are handled differently.
*)
rule general_scanner = parse
	| env_command		{Tok_env_comm lexbuf}
	| simple_command	{Tok_simple_comm lexbuf}
	| begin_marker		{Tok_begin lexbuf}
	| end_marker		{Tok_end lexbuf}
	| eof			{Tok_eof lexbuf}
	| break			{incr_linenum lexbuf; Tok_break}
	| space | eol		{incr_linenum lexbuf; Tok_space}
	| escape _		{incr_linenum lexbuf; Tok_plain (String.sub (Lexing.lexeme lexbuf) 1 1)}
	| entity		{Tok_entity (String.slice ~first:1 ~last:(-1) (Lexing.lexeme lexbuf))}
	| _			{Tok_plain (String.sub (Lexing.lexeme lexbuf) 0 1)}


(**	Scanner used for tabular environments.  It builds on the general scanner,
	but adding checks for the characters that separate columns and terminate rows.
*)
and tabular_scanner = parse
	| env_command		{Tok_env_comm lexbuf}
	| simple_command	{Tok_simple_comm lexbuf}
	| begin_marker		{Tok_begin lexbuf}
	| end_marker		{Tok_end lexbuf}
	| column_sep		{Tok_column_sep lexbuf}
	| row_end		{Tok_row_end lexbuf}
	| eof			{Tok_eof lexbuf}
	| break			{incr_linenum lexbuf; Tok_break}
	| space | eol		{incr_linenum lexbuf; Tok_space}
	| escape _		{incr_linenum lexbuf; Tok_plain (String.sub (Lexing.lexeme lexbuf) 1 1)}
	| entity		{Tok_entity (String.slice ~first:1 ~last:(-1) (Lexing.lexeme lexbuf))}
	| _			{Tok_plain (String.sub (Lexing.lexeme lexbuf) 0 1)}


(**	Special scanner for verbatim environments.  Pretty much every character
	is returned as text; the exceptions are the EOF character, the special
	"\end{verbatim}" termination tag, and HTML entities.
*)
and verbatim_scanner = parse
	| "\\end{verbatim}"	{Tok_env_comm lexbuf}
	| eof			{Tok_eof lexbuf}
	| entity		{Tok_entity (String.slice ~first:1 ~last:(-1) (Lexing.lexeme lexbuf))}
	| _			{incr_linenum lexbuf; Tok_plain (String.sub (Lexing.lexeme lexbuf) 0 1)}


(**	Special scanner for math environments.  No attempt whatsoever is made to
	interpret the characters in the stream.  This scanner only pays attention
	to the EOF character and the environment terminator "\end{math}".
*)
and math_scanner = parse
	| "\\end{math}"		{Tok_env_comm lexbuf}
	| eof			{Tok_eof lexbuf}
	| _			{incr_linenum lexbuf; Tok_plain (String.sub (Lexing.lexeme lexbuf) 0 1)}

