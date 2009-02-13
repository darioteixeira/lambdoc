(********************************************************************************)
(*	Implementation file for Document_reader.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Document reader.
*)

open Lambdoc_core
open Basic


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

(**	The module type that all wannabe document readers must export.
*)
module type READER =
sig
	exception Parsing_error of int
	exception Unknown_env_command of int * tag_t
	exception Unknown_simple_command of int * tag_t

	val ast_from_string: string -> Ast.t
end


(**	The signature exported by the functor.
*)
module type S =
sig
	val ambivalent_manuscript_from_string:
		?classnames: string list ->
		?accept_list: Features.manuscript_feature_t list ->
		?deny_list: Features.manuscript_feature_t list ->
		?default: Features.default_t ->
		string ->
		Ambivalent.manuscript_t

	val ambivalent_composition_from_string:
		?classnames: string list ->
		?accept_list: Features.composition_feature_t list ->
		?deny_list: Features.composition_feature_t list ->
		?default: Features.default_t ->
		string ->
		Ambivalent.composition_t
end


(********************************************************************************)
(**	{2 Modules and functors}						*)
(********************************************************************************)

(**	The functor that creates a document reader.
*)
module Make_reader (Reader: READER): S =
struct
	let ambivalent_document_from_string ?classnames ?accept_list ?deny_list ?default valid_processor invalid_maker str =
		try
			let document_ast = Reader.ast_from_string str
			in valid_processor ?classnames ?accept_list ?deny_list ?default str document_ast
		with
			| Reader.Parsing_error line ->
				let errors = Postprocess.collate_errors str [(line, Error.Syntax_error)]
				in invalid_maker errors
			| Reader.Unknown_env_command (line, tag) ->
				let errors = Postprocess.collate_errors str [(line, Error.Unknown_env_command tag)]
				in invalid_maker errors
			| Reader.Unknown_simple_command (line, tag) ->
				let errors = Postprocess.collate_errors str [(line, Error.Unknown_simple_command tag)]
				in invalid_maker errors

	let ambivalent_manuscript_from_string ?classnames ?accept_list ?deny_list ?default str =
		let valid_processor = Postprocess.process_manuscript
		and invalid_maker = Ambivalent.make_invalid_manuscript
		in ambivalent_document_from_string ?classnames ?accept_list ?deny_list ?default valid_processor invalid_maker str

	let ambivalent_composition_from_string ?classnames ?accept_list ?deny_list ?default str =
		let valid_processor = Postprocess.process_composition
		and invalid_maker = Ambivalent.make_invalid_composition
		in ambivalent_document_from_string ?classnames ?accept_list ?deny_list ?default valid_processor invalid_maker str
end

