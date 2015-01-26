(********************************************************************************)
(*	Lambdoc_rlambtex_reader.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Main interface to the Lambtex reader.
*)

open Lambdoc_rlambtex


(********************************************************************************)
(**	{1 Public modules}							*)
(********************************************************************************)

module Readable =
struct
	open Lexing


	(************************************************************************)
	(*	{2 Exceptions}							*)
	(************************************************************************)

	exception Reading_error of int * string


	(************************************************************************)
	(*	{2 Private functions and values}				*)
	(************************************************************************)

	let menhir_with_ulex menhir_parser tokenizer lexbuf =
		let lexer_maker () =
			let ante_position = tokenizer#position in
			let token = tokenizer#consume lexbuf in
			let post_position = tokenizer#position in
			(token, ante_position, post_position) in
		let revised_parser = MenhirLib.Convert.Simplified.traditional2revised menhir_parser in
		revised_parser lexer_maker


	(************************************************************************)
	(*	{2 Public functions and values}					*)
	(************************************************************************)

	let ast_from_string ~linenum_offset ~inline_extdefs ~block_extdefs str =
		let tokenizer = new Tokenizer.tokenizer ~linenum_offset ~inline_extdefs ~block_extdefs in
		try
			let lexbuf = Ulexing.from_utf8_string str in
			`Okay (menhir_with_ulex Parser.document tokenizer lexbuf)
		with exc ->
			let msg = match exc with
				| Utf8.MalFormed ->
					"Malformed UTF-8 sequence"
				| Globalenv.Pop_mismatch (found, expected) ->
					Printf.sprintf "Invalid closing for environment command: found '%s' but expected '%s'" found expected
				| Parser.Error ->
					"Syntax error"
				| exc ->
					raise exc
			in `Error [(Some tokenizer#position.pos_lnum, msg)]
end

module Make = Lambdoc_reader.Maker.Make (Readable)

module Trivial = Make (Lambdoc_reader.Extension.Trivial)

