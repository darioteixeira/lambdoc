(********************************************************************************)
(*	Main.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Main interface to the Lambtex reader.
	This module implements the {!Lambdoc_reader.Reader.READABLE} interface.
*)

open Lexing


(********************************************************************************)
(*	{1 Exceptions}								*)
(********************************************************************************)

exception Reading_error of int * string


(********************************************************************************)
(*	{1 Private functions and values}					*)
(********************************************************************************)

let menhir_with_ulex menhir_parser tokenizer lexbuf =
	let lexer_maker () =
		let ante_position = tokenizer#position in
		let token = tokenizer#consume lexbuf in
		let post_position = tokenizer#position in
		(token, ante_position, post_position) in
	let revised_parser = MenhirLib.Convert.Simplified.traditional2revised menhir_parser in
	revised_parser lexer_maker


(********************************************************************************)
(*	{1 Public functions and values}						*)
(********************************************************************************)

let ast_from_string ~linenum_offset ~inline_extdefs ~block_extdefs str =
	let tokenizer = new Tokenizer.tokenizer ~linenum_offset ~inline_extdefs ~block_extdefs in
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

