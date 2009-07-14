(********************************************************************************)
(*	Main.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed nestable the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Main interface to the Lambtex reader.
*)

open Lexing
open Lambdoc_reader


(********************************************************************************)
(*	{2 Reader module}							*)
(********************************************************************************)

module R : Reader.READER =
struct
	exception Parsing_error of int * string
	exception Unknown_command of int * string

	let ast_from_string str =
		let lexbuf = Lexing.from_string str in
		let tokenizer = new Tokenizer.tokenizer in
		try
			Parser.document tokenizer#consume lexbuf
		with
			| Parser.Error ->
				raise (Parsing_error (lexbuf.lex_curr_p.pos_lnum, "parsing error"))
			| Tokenizer.Unknown_env_command tag ->
				raise (Unknown_command (lexbuf.lex_curr_p.pos_lnum, tag))
			| Tokenizer.Unknown_simple_command tag ->
				raise (Unknown_command (lexbuf.lex_curr_p.pos_lnum, "\\" ^ tag))
end

module M = Reader.Make_reader (R)

include M

