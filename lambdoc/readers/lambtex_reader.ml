(********************************************************************************)
(*	Main interface to the Lambtex reader.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed nestable the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lexing
open Lambtex_reader_impl


(********************************************************************************)
(*	{2 Reader module}							*)
(********************************************************************************)

module Reader : Document_reader.READER =
struct
	exception Parsing_error of int
	exception Unknown_env_command of int * string
	exception Unknown_simple_command of int * string

	let ast_from_string str =
		let lexbuf = Lexing.from_string str in
		let tokenizer = new Lambtex_tokenizer.tokenizer in
		try
			Lambtex_parser.document tokenizer#consume lexbuf
		with
			| Lambtex_parser.Error ->
				raise (Parsing_error lexbuf.lex_curr_p.pos_lnum)
			| Lambtex_tokenizer.Unknown_env_command tag ->
				raise (Unknown_env_command (lexbuf.lex_curr_p.pos_lnum, tag))
			| Lambtex_tokenizer.Unknown_simple_command tag ->
				raise (Unknown_simple_command (lexbuf.lex_curr_p.pos_lnum, tag))
end

module M = Document_reader.Make_reader (Reader)

include M

