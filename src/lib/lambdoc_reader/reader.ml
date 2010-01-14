(********************************************************************************)
(*	Reader.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Document reader.
*)

open Lambdoc_core
open Basic


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
	val ambivalent_manuscript_from_string:
		?verify_utf8: bool ->
		?accept_list: Features.manuscript_feature_t list ->
		?deny_list: Features.manuscript_feature_t list ->
		?default: Features.default_t ->
		string ->
		Ambivalent.manuscript_t

	val ambivalent_composition_from_string:
		?verify_utf8: bool ->
		?accept_list: Features.composition_feature_t list ->
		?deny_list: Features.composition_feature_t list ->
		?default: Features.default_t ->
		string ->
		Ambivalent.composition_t
end


(********************************************************************************)
(**	{1 Modules and functors}						*)
(********************************************************************************)

(**	The functor that creates a document reader.
*)
module Make_reader (Reader: READER): S =
struct
	let ambivalent_document_from_string
		?(verify_utf8 = true)
		?(accept_list = [])
		?(deny_list = [])
		?(default = `Accept)
		~valid_compiler
		~invalid_maker
		source =
			try
				let () = if verify_utf8 then Preprocessor.verify_utf8 source in
				let document_ast = Reader.ast_from_string source
				in valid_compiler ~accept_list ~deny_list ~default ~source document_ast
			with
				| Preprocessor.Malformed_source (sane_str, error_lines) ->
					let msgs = List.map (fun line -> (Some line, Error.Malformed_code_point)) error_lines in
					let errors = Compiler.process_errors ~sort:false sane_str msgs
					in invalid_maker errors
				| Reader.Reading_error (line, msg) ->
					let errors = Compiler.process_errors ~sort:false source [(Some line, Error.Reading_error msg)]
					in invalid_maker errors

	let ambivalent_manuscript_from_string ?verify_utf8 ?accept_list ?deny_list ?default source =
		let valid_compiler = Compiler.compile_manuscript
		and invalid_maker = Ambivalent.make_invalid_manuscript
		in ambivalent_document_from_string ?verify_utf8 ?accept_list ?deny_list ?default ~valid_compiler ~invalid_maker source

	let ambivalent_composition_from_string ?verify_utf8 ?accept_list ?deny_list ?default source =
		let valid_compiler = Compiler.compile_composition
		and invalid_maker = Ambivalent.make_invalid_composition
		in ambivalent_document_from_string ?verify_utf8 ?accept_list ?deny_list ?default ~valid_compiler ~invalid_maker source
end

