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
	| Tok_env_command of Lexing.lexbuf
	| Tok_simple_command of Lexing.lexbuf
	| Tok_begin of Lexing.lexbuf
	| Tok_end of Lexing.lexbuf
	| Tok_begin_mathtex_inl of Lexing.lexbuf
	| Tok_end_mathtex_inl of Lexing.lexbuf
	| Tok_begin_mathml_inl of Lexing.lexbuf
	| Tok_end_mathml_inl of Lexing.lexbuf
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
let begin_mathtex_inl = "[$"
let end_mathtex_inl = "$]"
let begin_mathml_inl = "<$"
let end_mathml_inl = "$>"
let column_sep = space* '|' space*
let row_end = space* '$'


(********************************************************************************)
(**	{2 Actual scanners}							*)
(********************************************************************************)

(**	There are eight possible scanning contexts in a Lambtex document, each
	demanding special rules from the scanner.  While some of the contexts
	are so similar they could in theory be handled by the same scanner given
	one differentiating parameter, in practice because the lexer generators
	we are using cannot create parameterised lexers, we use eight different
	scanner functions.  Note that in all scanners, some actions could have
	been simplified if we had used Ocamllex's "as" operator.  We chose not
	to do so because Ulex has no equivalent to this operator, and we prefer
	code that is easily adapted to different lexer generators.
*)

(**	The default scanner for documents.  This scanner is used in Inline
	and Block contexts.  Note that the backslash is used as an escape
	character and HTML entities are also recognised.  Also noteworthy
	is that spacing and line breaks are handled differently.
*)
rule general_scanner = parse
	| env_command		{Tok_env_command lexbuf}
	| simple_command	{Tok_simple_command lexbuf}
	| begin_marker		{Tok_begin lexbuf}
	| end_marker		{Tok_end lexbuf}
	| begin_mathtex_inl	{Tok_begin_mathtex_inl lexbuf}
	| begin_mathml_inl	{Tok_begin_mathml_inl lexbuf}
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
	| env_command		{Tok_env_command lexbuf}
	| simple_command	{Tok_simple_command lexbuf}
	| begin_marker		{Tok_begin lexbuf}
	| end_marker		{Tok_end lexbuf}
	| begin_mathtex_inl	{Tok_begin_mathtex_inl lexbuf}
	| begin_mathml_inl	{Tok_begin_mathml_inl lexbuf}
	| column_sep		{Tok_column_sep lexbuf}
	| row_end		{Tok_row_end lexbuf}
	| eof			{Tok_eof lexbuf}
	| break			{incr_linenum lexbuf; Tok_break}
	| space | eol		{incr_linenum lexbuf; Tok_space}
	| escape _		{incr_linenum lexbuf; Tok_plain (String.sub (Lexing.lexeme lexbuf) 1 1)}
	| entity		{Tok_entity (String.slice ~first:1 ~last:(-1) (Lexing.lexeme lexbuf))}
	| _			{Tok_plain (String.sub (Lexing.lexeme lexbuf) 0 1)}


(**	Special scanner for code environments.  Pretty much every character
	is returned as text; the exceptions are the EOF character, the special
	"\end{code}" termination tag, and HTML entities.
*)
and code_scanner = parse
	| "\\end{code}"		{Tok_env_command lexbuf}
	| eof			{Tok_eof lexbuf}
	| entity		{Tok_entity (String.slice ~first:1 ~last:(-1) (Lexing.lexeme lexbuf))}
	| _			{incr_linenum lexbuf; Tok_plain (String.sub (Lexing.lexeme lexbuf) 0 1)}


(**	Special scanner for verbatim environments.  Pretty much every character
	is returned as text; the exceptions are the EOF character, the special
	"\end{verbatim}" termination tag, and HTML entities.
*)
and verbatim_scanner = parse
	| "\\end{verbatim}"	{Tok_env_command lexbuf}
	| eof			{Tok_eof lexbuf}
	| entity		{Tok_entity (String.slice ~first:1 ~last:(-1) (Lexing.lexeme lexbuf))}
	| _			{incr_linenum lexbuf; Tok_plain (String.sub (Lexing.lexeme lexbuf) 0 1)}


(**	Special scanner for mathtex environments in an inline context.  No attempt whatsoever
	is made to interpret the characters in the stream.  This scanner only pays attention
	to the EOF character and the terminator "$]".
*)
and mathtex_inl_scanner = parse
	| end_mathtex_inl	{Tok_end_mathtex_inl lexbuf}
	| eof			{Tok_eof lexbuf}
	| _			{incr_linenum lexbuf; Tok_plain (String.sub (Lexing.lexeme lexbuf) 0 1)}


(**	Special scanner for mathml environments in an inline context.  No attempt whatsoever
	is made to interpret the characters in the stream.  This scanner only pays attention
	to the EOF character and the terminator "$>".
*)
and mathml_inl_scanner = parse
	| end_mathml_inl	{Tok_end_mathml_inl lexbuf}
	| eof			{Tok_eof lexbuf}
	| _			{incr_linenum lexbuf; Tok_plain (String.sub (Lexing.lexeme lexbuf) 0 1)}


(**	Special scanner for mathtex environments in a block context.  No attempt whatsoever
	is made to interpret the characters in the stream.  This scanner only pays attention
	to the EOF character and the environment terminator "\end{tex}".
*)
and mathtex_blk_scanner = parse
	| "\\end{tex}"		{Tok_env_command lexbuf}
	| eof			{Tok_eof lexbuf}
	| _			{incr_linenum lexbuf; Tok_plain (String.sub (Lexing.lexeme lexbuf) 0 1)}


(**	Special scanner for mathml environments in a block context.  No attempt whatsoever
	is made to interpret the characters in the stream.  This scanner only pays attention
	to the EOF character and the environment terminator "\end{mathml}".
*)
and mathml_blk_scanner = parse
	| "\\end{mathml}"	{Tok_env_command lexbuf}
	| eof			{Tok_eof lexbuf}
	| _			{incr_linenum lexbuf; Tok_plain (String.sub (Lexing.lexeme lexbuf) 0 1)}

