(********************************************************************************)
(*	Reader.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Document reader.
*)

open Lambdoc_core


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

(**	The module type that all wannabe document readers must export.
*)
module type READER =
sig
	exception Reading_error of int * string

	val ast_from_string: string -> Ast.t
end


(**	The signature exported by the functor.
*)
module type S =
sig
	val ambivalent_from_string:
		?bookmaker:Bookmaker.t ->
		?verify_utf8:bool ->
		?expand_entities:bool ->
		?feature_ruleset:Features.feature_ruleset_t ->
		?feature_default:Features.action_t ->
		?classname_ruleset:Features.classname_ruleset_t ->
		?classname_default:Features.action_t ->
		string ->
		Ambivalent.t
end


(********************************************************************************)
(**	{1 Modules and functors}						*)
(********************************************************************************)

(**	The functor that creates a document reader.
*)
module Make_reader (Reader: READER): S =
struct
	let ambivalent_from_string
		?bookmaker
		?(verify_utf8 = true)
		?(expand_entities = true)
		?(feature_ruleset = [])
		?(feature_default = `Accept)
		?(classname_ruleset = [])
		?(classname_default = `Accept)
		source =
			try
				let () = if verify_utf8 then Preprocessor.verify_utf8 source in
				let ast = Reader.ast_from_string source in
				Compiler.compile ?bookmaker ~expand_entities ~feature_ruleset ~feature_default ~classname_ruleset ~classname_default ~source ast
			with
				| Preprocessor.Malformed_source (sane_str, error_lines) ->
					let msgs = List.map (fun line -> (Some line, Error.Malformed_code_point)) error_lines in
					let errors = Compiler.process_errors ~sort:false sane_str msgs in
					Ambivalent.make_invalid errors
				| Reader.Reading_error (line, msg) ->
					let errors = Compiler.process_errors ~sort:false source [(Some line, Error.Reading_error msg)] in
					Ambivalent.make_invalid errors
end

