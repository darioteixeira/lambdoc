(********************************************************************************)
(*	Scanner.ml
	Copyright (c) 2009-2013 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Scanner for the Lambtex reader.
*)

module String = BatString


(********************************************************************************)
(**	{1 Auxiliary types}							*)
(********************************************************************************)

(**	The set of all tokens output by the various scanners.
*)

type tok_simple_comm_t =	[ `Tok_simple_comm of string ]
type tok_env_begin_t =		[ `Tok_env_begin of string ]
type tok_env_end_t =		[ `Tok_env_end of string ]
type tok_begin_t =		[ `Tok_begin ]
type tok_end_t =		[ `Tok_end ]
type tok_begin_mathtex_inl_t =	[ `Tok_begin_mathtex_inl ]
type tok_end_mathtex_inl_t =	[ `Tok_end_mathtex_inl ]
type tok_begin_mathml_inl_t =	[ `Tok_begin_mathml_inl ]
type tok_end_mathml_inl_t =	[ `Tok_end_mathml_inl ]
type tok_cell_mark_t =		[ `Tok_cell_mark ]
type tok_row_end_t =		[ `Tok_row_end ]
type tok_eof_t =		[ `Tok_eof ]
type tok_parbreak_t =		[ `Tok_parbreak ]
type tok_space_t =		[ `Tok_space ]
type tok_raw_t =		[ `Tok_raw of string ]
type tok_plain_t =		[ `Tok_plain of string ]
type tok_entity_t =		[ `Tok_entity of string ]

type general_token_t =
	[ tok_simple_comm_t
	| tok_env_begin_t | tok_env_end_t
	| tok_begin_t | tok_end_t
	| tok_begin_mathtex_inl_t
	| tok_begin_mathml_inl_t
	| tok_eof_t | tok_parbreak_t
	| tok_space_t | tok_plain_t | tok_entity_t
	]

type tabular_token_t =
	[ tok_simple_comm_t
	| tok_env_begin_t | tok_env_end_t
	| tok_begin_t | tok_end_t
	| tok_begin_mathtex_inl_t
	| tok_begin_mathml_inl_t
	| tok_cell_mark_t | tok_row_end_t
	| tok_eof_t | tok_parbreak_t
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
(**	{1 List of regular expressions used in the scanners}			*)
(********************************************************************************)

let regexp begin_marker = '{'
let regexp end_marker = '}'

let regexp begin_mathtex_inl = "[$"
let regexp end_mathtex_inl = "$]"
let regexp begin_mathml_inl = "<$"
let regexp end_mathml_inl = "$>"

let regexp lower = ['a'-'z']
let regexp alpha = ['a'-'z' 'A'-'Z']
let regexp deci = ['0'-'9']
let regexp ident = lower (lower | deci | '_')*

let regexp order = '(' [^ ')' '[' ']' '<' '>' '{' '}']* ')'
let regexp label = '[' [^ ']' '(' ')' '<' '>' '{' '}']* ']'
let regexp style = '<' [^ '>' '[' ']' '(' ')' '{' '}']* '>'
let regexp optional = ( order | label | style )*
let regexp primary = '{' ident '}'

let regexp simple_comm = '\\' ident optional
let regexp env_begin = "\\begin" optional primary
let regexp env_end = "\\end" primary

let regexp newline = "\r\n" | '\n'
let regexp space = [' ' '\t']
let regexp escape = '\\'
let regexp eol = space* newline space*
let regexp parbreak = eol eol+

let regexp cell_mark = space* '|' space*
let regexp row_end = space* '|' space* newline

let regexp entity = '&' '#'? (alpha | deci)+ ';'
let regexp endash = "--"
let regexp emdash = "---"
let regexp quote_open = "``"
let regexp quote_close = "''"


(********************************************************************************)
(**	{1 Auxiliary functions}							*)
(********************************************************************************)

let count_lines lexbuf =
	let adder acc el = if el = 0x0a then acc+1 else acc in	(* We expect lines to be terminated with '\r\n' or just '\n' *)
	let lexeme = Ulexing.lexeme lexbuf in
	Array.fold_left adder 0 lexeme

let whole_lexbuf lexbuf =
	Ulexing.utf8_lexeme lexbuf

let sub_lexbuf ~pos ~len lexbuf =
	Ulexing.utf8_sub_lexeme lexbuf pos len

let rtrim_lexbuf ~first lexbuf =
	Ulexing.utf8_sub_lexeme lexbuf first ((Ulexing.lexeme_length lexbuf) - first - 1)


(********************************************************************************)
(**	{1 Actual scanners}							*)
(********************************************************************************)

(**	There are seven possible scanning environments in a Lambtex document,
	each demanding special rules from the scanner.  While some of the contexts
	are so similar they could in theory be handled by the same scanner given
	one differentiating parameter, in practice because the lexer generator
	we are using cannot create parameterised lexers, we use seven different
	scanner functions.  Also of note is that the return is a pair consisting
	of an integer and the scanned token.  The integer indicates the number
	of newline characters found in the token.
*)

(**	General document scanner.
*)
let general_scanner : (Ulexing.lexbuf -> int * [> general_token_t]) = lexer
	| simple_comm		-> (0, `Tok_simple_comm (whole_lexbuf lexbuf))
	| env_begin		-> (0, `Tok_env_begin (whole_lexbuf lexbuf))
	| env_end		-> (0, `Tok_env_end (rtrim_lexbuf ~first:5 lexbuf))
	| begin_marker		-> (0, `Tok_begin)
	| end_marker		-> (0, `Tok_end)
	| begin_mathtex_inl	-> (0, `Tok_begin_mathtex_inl)
	| end_mathtex_inl	-> (0, `Tok_end_mathtex_inl)
	| begin_mathml_inl	-> (0, `Tok_begin_mathml_inl)
	| end_mathml_inl	-> (0, `Tok_end_mathml_inl)
	| eof			-> (0, `Tok_eof)
	| parbreak		-> (count_lines lexbuf, `Tok_parbreak)
	| space+ | eol		-> (count_lines lexbuf, `Tok_space)
	| escape _		-> (count_lines lexbuf, `Tok_plain (sub_lexbuf ~pos:1 ~len:1 lexbuf))
	| entity		-> (0, `Tok_entity (rtrim_lexbuf ~first:1 lexbuf))
	| endash		-> (0, `Tok_entity "ndash")
	| emdash		-> (0, `Tok_entity "mdash")
	| quote_open		-> (0, `Tok_entity "ldquo")
	| quote_close		-> (0, `Tok_entity "rdquo")
	| _			-> (0, `Tok_plain (whole_lexbuf lexbuf))


(**	Scanner used for tabular environments.  It is mostly identical
	to the inline scanner, but scans for characters that separate
	columns and terminate rows.
*)
let tabular_scanner : (Ulexing.lexbuf -> int * [> tabular_token_t]) = lexer
	| simple_comm		-> (0, `Tok_simple_comm (whole_lexbuf lexbuf))
	| env_begin		-> (0, `Tok_env_begin (whole_lexbuf lexbuf))
	| env_end		-> (0, `Tok_env_end (rtrim_lexbuf ~first:5 lexbuf))
	| begin_marker		-> (0, `Tok_begin)
	| end_marker		-> (0, `Tok_end)
	| begin_mathtex_inl	-> (0, `Tok_begin_mathtex_inl)
	| end_mathtex_inl	-> (0, `Tok_end_mathtex_inl)
	| begin_mathml_inl	-> (0, `Tok_begin_mathml_inl)
	| end_mathml_inl	-> (0, `Tok_end_mathml_inl)
	| cell_mark		-> (0, `Tok_cell_mark)			(* new compared to general *)
	| row_end		-> (count_lines lexbuf, `Tok_row_end)	(* new compared to general *)
	| eof			-> (0, `Tok_eof)
	| parbreak		-> (count_lines lexbuf, `Tok_parbreak)
	| space+ | eol		-> (count_lines lexbuf, `Tok_space)
	| escape _		-> (count_lines lexbuf, `Tok_plain (sub_lexbuf ~pos:1 ~len:1 lexbuf))
	| entity		-> (0, `Tok_entity (rtrim_lexbuf ~first:1 lexbuf))
	| endash		-> (0, `Tok_entity "ndash")
	| emdash		-> (0, `Tok_entity "mdash")
	| quote_open		-> (0, `Tok_entity "ldquo")
	| quote_close		-> (0, `Tok_entity "rdquo")
	| _			-> (0, `Tok_plain (whole_lexbuf lexbuf))


(**	Special scanner for raw environments.  Pretty much every character is
	returned as raw text; the exceptions are the EOF character, escaped
	characters, and the special "}" termination tag.
*)
let raw_scanner : (Ulexing.lexbuf -> int * [> raw_token_t]) = lexer
	| end_marker		-> (0, `Tok_end)
	| eof			-> (0, `Tok_eof)
	| escape _		-> (count_lines lexbuf, `Tok_raw (sub_lexbuf ~pos:1 ~len:1 lexbuf))
	| _			-> (count_lines lexbuf, `Tok_raw (whole_lexbuf lexbuf))


(**	Special scanner for mathtex environments in an inline context.  No attempt
	whatsoever is made to interpret the characters in the stream.  This scanner
	only pays attention to the EOF character and the terminator "$]".
*)
let mathtex_inl_scanner : (Ulexing.lexbuf -> int * [> mathtex_inl_token_t]) = lexer
	| end_mathtex_inl	-> (0, `Tok_end_mathtex_inl)
	| eof			-> (0, `Tok_eof)
	| _			-> (count_lines lexbuf, `Tok_raw (whole_lexbuf lexbuf))


(**	Special scanner for mathml environments in an inline context.  No attempt
	whatsoever is made to interpret the characters in the stream.  This scanner
	only pays attention to the EOF character and the terminator "$>".
*)
let mathml_inl_scanner : (Ulexing.lexbuf -> int * [> mathml_inl_token_t]) = lexer
	| end_mathml_inl	-> (0, `Tok_end_mathml_inl)
	| eof			-> (0, `Tok_eof)
	| _			-> (count_lines lexbuf, `Tok_raw (whole_lexbuf lexbuf))


(**	Special parametrised scanner for verbatim-like environments.  The parameter
	indicates the termination token for the environment.  The actual Lambtex
	environments using it are "verbatim", "source", "mathtex", and "mathml".
*)
let literal_scanner terminator : (Ulexing.lexbuf -> int * [> literal_token_t]) = lexer
	| env_end ->
		let str = rtrim_lexbuf ~first:5 lexbuf in
		if str = terminator
		then (0, `Tok_env_end (rtrim_lexbuf ~first:5 lexbuf))
		else (0, `Tok_raw (whole_lexbuf lexbuf))
	| eof ->
		(0, `Tok_eof)
	| _ ->
		(count_lines lexbuf, `Tok_raw (whole_lexbuf lexbuf))

