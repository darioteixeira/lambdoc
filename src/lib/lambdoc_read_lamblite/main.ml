(********************************************************************************)
(*	Main.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed nestable the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Main interface to the Lamblite reader.
*)

open Lexing
open Lambdoc_reader


(********************************************************************************)
(*	{2 Reader module}							*)
(********************************************************************************)

module R : Reader.READER =
struct
	exception Reading_error of int * string

	let menhir_with_ulex menhir_parser tokenizer =
		let lexer_maker () =
			let ante_position = tokenizer#position in
			let token = tokenizer#consume in
			let post_position = tokenizer#position
			in (token, ante_position, post_position) in
		let revised_parser = MenhirLib.Convert.Simplified.traditional2revised menhir_parser
		in revised_parser lexer_maker

	let ast_from_string str =
		let tokenizer = new Tokenizer.tokenizer str
		in try
			menhir_with_ulex Parser.document tokenizer
		with
			| Scanner.Lone_terminator ->
				raise (Reading_error (tokenizer#position.pos_lnum, "Syntax error"))
			| Tokenizer.Invalid_section_level num ->
				let msg = Printf.sprintf "You have requested %d levels of sectioning, but only 3 are available" num
				in raise (Reading_error (tokenizer#position.pos_lnum, msg))
			| Parser.Error ->
				raise (Reading_error (tokenizer#position.pos_lnum, "Syntax error"))
end

module M = Reader.Make_reader (R)

include M

