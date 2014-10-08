(********************************************************************************)
(*	Main.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Main interface to the Lambwiki reader.
*)

open Lexing
open Lambdoc_reader


(********************************************************************************)
(*	{2 Reader module}							*)
(********************************************************************************)

module Make = Reader.Make
(struct
	exception Reading_error of int * string

	let menhir_with_ulex menhir_parser tokenizer =
		let lexer_maker () =
			let ante_position = tokenizer#position in
			let token = tokenizer#consume in
			let post_position = tokenizer#position in
			(token, ante_position, post_position) in
		let revised_parser = MenhirLib.Convert.Simplified.traditional2revised menhir_parser in
		revised_parser lexer_maker

	let ast_from_string str =
		let tokenizer = new Tokenizer.tokenizer str in
		try
			menhir_with_ulex Parser.document tokenizer
		with
			| Tokenizer.Invalid_ulist_level (current, found) ->
				let msg = Printf.sprintf "You've requested an unordered list %d levels deep, but in this context a maximum of %d is allowed in this context" found (current+1) in
				raise (Reading_error (tokenizer#position.pos_lnum, msg))
			| Tokenizer.Invalid_olist_level (current, found) ->
				let msg = Printf.sprintf "You've requested an ordered list %d levels deep, but a maximum of %d is allowed in this context" found (current+1) in
				raise (Reading_error (tokenizer#position.pos_lnum, msg))
			| Parser.Error ->
				raise (Reading_error (tokenizer#position.pos_lnum, "Syntax error"))
end)

