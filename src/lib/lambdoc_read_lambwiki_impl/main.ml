(********************************************************************************)
(*	Main.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Main interface to the Lambwiki reader.
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

let menhir_with_ulex menhir_parser tokenizer =
	let lexer_maker () =
		let ante_position = tokenizer#position in
		let token = tokenizer#consume in
		let post_position = tokenizer#position in
		(token, ante_position, post_position) in
	let revised_parser = MenhirLib.Convert.Simplified.traditional2revised menhir_parser in
	revised_parser lexer_maker


(********************************************************************************)
(*	{1 Public functions and values}						*)
(********************************************************************************)

let ast_from_string ~linenum_offset ~inline_extdefs ~block_extdefs str =
	let tokenizer = new Tokenizer.tokenizer ~linenum_offset str in
	try
		`Okay (menhir_with_ulex Parser.document tokenizer)
	with exc ->
		let msg = match exc with
			| Tokenizer.Invalid_ulist_level (current, found) ->
				Printf.sprintf "You've requested an unordered list %d levels deep, but a maximum of %d is allowed in this context" found (current+1)
			| Tokenizer.Invalid_olist_level (current, found) ->
				Printf.sprintf "You've requested an ordered list %d levels deep, but a maximum of %d is allowed in this context" found (current+1)
			| Parser.Error ->
				"Syntax error"
			| exc ->
				raise exc
		in `Error [(Some tokenizer#position.pos_lnum, msg)]

