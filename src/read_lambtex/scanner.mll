(********************************************************************************)
(*	Implementation file for Lambtex_scanner.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Scanner for the Lambtex reader.
*)

(********************************************************************************)
(**	{2 Auxiliary types}							*)
(********************************************************************************)

{
open Lexing
open ExtString
open Parser


(**	The set of all tokens output by the various scanners.
*)

type tok_simple_comm_t =	[ `Tok_simple_comm of Lexing.lexbuf ]
type tok_env_begin_t =		[ `Tok_env_begin of Lexing.lexbuf ]
type tok_env_end_t =		[ `Tok_env_end of Lexing.lexbuf ]
type tok_begin_t =		[ `Tok_begin ]
type tok_end_t =		[ `Tok_end ]
type tok_begin_mathtex_inl_t =	[ `Tok_begin_mathtex_inl of Lexing.lexbuf ]
type tok_end_mathtex_inl_t =	[ `Tok_end_mathtex_inl of Lexing.lexbuf ]
type tok_begin_mathml_inl_t =	[ `Tok_begin_mathml_inl of Lexing.lexbuf ]
type tok_end_mathml_inl_t =	[ `Tok_end_mathml_inl of Lexing.lexbuf ]
type tok_column_sep_t =		[ `Tok_column_sep of Lexing.lexbuf ]
type tok_row_end_t =		[ `Tok_row_end of Lexing.lexbuf ]
type tok_eof_t =		[ `Tok_eof ]
type tok_break_t =		[ `Tok_break ]
type tok_space_t =		[ `Tok_space ]
type tok_raw_t =		[ `Tok_raw of string ]
type tok_plain_t =		[ `Tok_plain of Lexing.lexbuf * string ]
type tok_entity_t =		[ `Tok_entity of Lexing.lexbuf * string ]

type general_token_t =
	[ tok_simple_comm_t
	| tok_env_begin_t | tok_env_end_t
	| tok_begin_t | tok_end_t
	| tok_begin_mathtex_inl_t
	| tok_begin_mathml_inl_t
	| tok_eof_t | tok_break_t
	| tok_space_t | tok_plain_t | tok_entity_t
	]

type tabular_token_t =
	[ tok_simple_comm_t
	| tok_env_begin_t | tok_env_end_t
	| tok_begin_t | tok_end_t
	| tok_begin_mathtex_inl_t
	| tok_begin_mathml_inl_t
	| tok_column_sep_t | tok_row_end_t
	| tok_eof_t | tok_break_t
	| tok_space_t | tok_plain_t | tok_entity_t
	]

type raw_token_t =
	[ tok_end_t
	| tok_eof_t
	| tok_raw_t
	]

type mathtex_inl_token_t =
	[ tok_end_mathtex_inl_t
	| tok_eof_t
	| tok_raw_t
	]

type mathml_inl_token_t =
	[ tok_end_mathml_inl_t
	| tok_eof_t
	| tok_raw_t
	]

type literal_token_t =
	[ tok_env_end_t
	| tok_eof_t
	| tok_raw_t
	]


(********************************************************************************)
(**	{2 Auxiliary functions}							*)
(********************************************************************************)

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
(**	{2 List of regular expressions used in the scanners}			*)
(********************************************************************************)

let alpha = ['a'-'z' 'A'-'Z']
let deci = ['0'-'9']
let order_char = alpha | deci | '.'
let label_char = alpha | deci | '-' | ':' | '_'
let extra_char = alpha

let order = '(' order_char* ')'
let label = '[' label_char+ ']'
let extra = '<' extra_char+ '>'
let optional = ( order | label | extra )*
let primary = '{' (alpha | '_')+ '}'
let secondary = '{' alpha* '}'

let simple_comm = '\\' alpha+ optional
let env_begin = "\\begin" optional primary secondary?
let env_end = "\\end" primary

let entity = '&' (alpha+ | ('#' deci+))  ';'
let space = [' ' '\t']
let escape = '\\'
let eol = space* '\n' space*
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

(**	There are seven possible scanning environments in a Lambtex document,
	each demanding special rules from the scanner.  While some of the contexts
	are so similar they could in theory be handled by the same scanner given
	one differentiating parameter, in practice because the lexer generators
	we are using cannot create parameterised lexers, we use seven different
	scanner functions.  Note that in all scanners, some actions could have
	been simplified if we had used Ocamllex's "as" operator.  We chose not
	to do so because Ulex has no equivalent to this operator, and we prefer
	code that is easily adapted to different lexer generators.
*)

(**	General document scanner.
*)
rule general_scanner = parse
	| simple_comm		{`Tok_simple_comm lexbuf}
	| env_begin		{`Tok_env_begin lexbuf}
	| env_end		{`Tok_env_end lexbuf}
	| begin_marker		{`Tok_begin}
	| end_marker		{`Tok_end}
	| begin_mathtex_inl	{`Tok_begin_mathtex_inl lexbuf}
	| begin_mathml_inl	{`Tok_begin_mathml_inl lexbuf}
	| eof			{`Tok_eof}
	| break			{incr_linenum lexbuf; `Tok_break}
	| space+ | eol		{incr_linenum lexbuf; `Tok_space lexbuf}
	| escape _		{incr_linenum lexbuf; `Tok_plain (lexbuf, (String.sub (Lexing.lexeme lexbuf) 1 1))}
	| entity		{`Tok_entity (lexbuf, (String.slice ~first:1 ~last:(-1) (Lexing.lexeme lexbuf)))}
	| _			{`Tok_plain (lexbuf, (String.sub (Lexing.lexeme lexbuf) 0 1))}


(**	Scanner used for tabular environments.  It is mostly identical
	to the inline scanner, but scans for characters that separate
	columns and terminate rows.
*)
and tabular_scanner = parse
	| simple_comm		{`Tok_simple_comm lexbuf}
	| env_begin		{`Tok_env_begin lexbuf}
	| env_end		{`Tok_env_end lexbuf}
	| begin_marker		{`Tok_begin}
	| end_marker		{`Tok_end}
	| begin_mathtex_inl	{`Tok_begin_mathtex_inl lexbuf}
	| begin_mathml_inl	{`Tok_begin_mathml_inl lexbuf}
	| column_sep		{`Tok_column_sep lexbuf}
	| row_end		{`Tok_row_end lexbuf}
	| eof			{`Tok_eof}
	| break			{incr_linenum lexbuf; `Tok_break}
	| space+ | eol		{incr_linenum lexbuf; `Tok_space lexbuf}
	| escape _		{incr_linenum lexbuf; `Tok_plain (lexbuf, (String.sub (Lexing.lexeme lexbuf) 1 1))}
	| entity		{`Tok_entity (lexbuf, (String.slice ~first:1 ~last:(-1) (Lexing.lexeme lexbuf)))}
	| _			{`Tok_plain (lexbuf, (String.sub (Lexing.lexeme lexbuf) 0 1))}


(**	Special scanner for raw environments.  Pretty much every character is
	returned as raw text; the exceptions are the EOF character, escaped
	characters, and the special "}" termination tag.
*)
and raw_scanner = parse
	| end_marker		{`Tok_end}
	| eof			{`Tok_eof}
	| escape _		{incr_linenum lexbuf; `Tok_raw (String.sub (Lexing.lexeme lexbuf) 1 1)}
	| _			{incr_linenum lexbuf; `Tok_raw (String.sub (Lexing.lexeme lexbuf) 0 1)}


(**	Special scanner for mathtex environments in an inline context.  No attempt
	whatsoever is made to interpret the characters in the stream.  This scanner
	only pays attention to the EOF character and the terminator "$]".
*)
and mathtex_inl_scanner = parse
	| end_mathtex_inl	{`Tok_end_mathtex_inl lexbuf}
	| eof			{`Tok_eof}
	| _			{incr_linenum lexbuf; `Tok_raw (String.sub (Lexing.lexeme lexbuf) 0 1)}


(**	Special scanner for mathml environments in an inline context.  No attempt
	whatsoever is made to interpret the characters in the stream.  This scanner
	only pays attention to the EOF character and the terminator "$>".
*)
and mathml_inl_scanner = parse
	| end_mathml_inl	{`Tok_end_mathml_inl lexbuf}
	| eof			{`Tok_eof}
	| _			{incr_linenum lexbuf; `Tok_raw (String.sub (Lexing.lexeme lexbuf) 0 1)}


(**	Special parametrised scanner for verbatim-like environments.  The parameter
	indicates the termination token for the environment.  The actual Lambtex
	environments using it are "verbatim", "code", "mathtex", and "mathml".
*)

and literal_scanner terminator = parse
	| env_end		{
				let str = String.slice ~first:5 ~last:(-1) (Lexing.lexeme lexbuf)
				in if str = terminator
				then `Tok_env_end lexbuf
				else `Tok_raw (Lexing.lexeme lexbuf)
				}
	| eof			{`Tok_eof}
	| _			{incr_linenum lexbuf; `Tok_raw (String.sub (Lexing.lexeme lexbuf) 0 1)}

