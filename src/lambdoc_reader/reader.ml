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
(**	{2 Type definitions}							*)
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
			let () = Preprocess.validate_utf8 str in
			let document_ast = Reader.ast_from_string str
			in valid_processor ?classnames ?accept_list ?deny_list ?default str document_ast
		with
			| Preprocess.Malformed_source (sane_str, error_lines) ->
				let msgs = List.map (fun line -> (line, Error.Malformed_code)) error_lines in
				let errors = Postprocess.collate_errors sane_str msgs
				in invalid_maker errors
			| Reader.Reading_error (line, msg) ->
				let errors = Postprocess.collate_errors str [(line, Error.Reading_error msg)]
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

