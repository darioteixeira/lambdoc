(********************************************************************************)
(*	Lambdoc_rlambtex_scanner.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Scanner for the Lambtex reader.
*)


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
(**	{1 Actual scanners}							*)
(********************************************************************************)

let general_scanner : (Lexing.lexbuf -> int * [> general_token_t]) = fun lexbuf -> (0, `Tok_eof)
let tabular_scanner : (Lexing.lexbuf -> int * [> tabular_token_t]) = fun lexbuf -> (0, `Tok_eof)
let raw_scanner : (Lexing.lexbuf -> int * [> raw_token_t]) = fun lexbuf -> (0, `Tok_eof)
let mathtex_inl_scanner : (Lexing.lexbuf -> int * [> mathtex_inl_token_t]) = fun lexbuf -> (0, `Tok_eof)
let mathml_inl_scanner : (Lexing.lexbuf -> int * [> mathml_inl_token_t]) = fun lexbuf -> (0, `Tok_eof)
let literal_scanner terminator : (Lexing.lexbuf -> int * [> literal_token_t]) = fun lexbuf -> (0, `Tok_eof)

