(********************************************************************************)
(*	Main.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
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
	exception Reading_error of int * string

	let menhir_with_ulex menhir_parser tokenizer lexbuf =
		let lexer_maker () =
			let ante_position = tokenizer#position in
			let token = tokenizer#consume lexbuf in
			let post_position = tokenizer#position in
			(token, ante_position, post_position) in
		let revised_parser = MenhirLib.Convert.Simplified.traditional2revised menhir_parser in
		revised_parser lexer_maker

	let ast_from_string str =
		let tokenizer = new Tokenizer.tokenizer in
		try
			let lexbuf = Ulexing.from_utf8_string str in
			menhir_with_ulex Parser.document tokenizer lexbuf
		with
			| Utf8.MalFormed ->
				raise (Reading_error (tokenizer#position.pos_lnum, "Malformed UTF-8 sequence"))
			| Globalenv.Pop_mismatch (found, expected) ->
				raise (Reading_error (tokenizer#position.pos_lnum, Printf.sprintf "Invalid closing for environment command: found '%s' but expected '%s'" found expected))
			| Parser.Error ->
				raise (Reading_error (tokenizer#position.pos_lnum, "Syntax error"))
end

include Reader.Make_reader (R)

