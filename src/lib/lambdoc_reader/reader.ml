(********************************************************************************)
(*	Reader.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core


(********************************************************************************)
(**	{1 Public signatures}							*)
(********************************************************************************)

module type READABLE =
sig
	exception Reading_error of int * string

	val ast_from_string: string -> Ast.t
end


module type S =
sig
	type 'a monad_t
	type link_t
	type image_t
	type extern_t

	val ambivalent_from_string:
		?verify_utf8:bool ->
		?expand_entities:bool ->
		?idiosyncrasies:Idiosyncrasies.t ->
		string ->
		(link_t, image_t, extern_t) Ambivalent.t monad_t
end


module type PARTIAL =
sig
	module Make: functor (Ext: Extension.S) -> S with
		type 'a monad_t = 'a Ext.Monad.t and
		type link_t = Ext.link_t and
		type image_t = Ext.image_t and
		type extern_t = Ext.extern_t
end


(********************************************************************************)
(**	{1 Public modules and functors}						*)
(********************************************************************************)

module Make (Readable: READABLE) (Ext: Extension.S) : S with
	type 'a monad_t = 'a Ext.Monad.t and
	type link_t = Ext.link_t and
	type image_t = Ext.image_t and
	type extern_t = Ext.extern_t =
struct
	open Ext

	type 'a monad_t = 'a Monad.t
	type link_t = Ext.link_t
	type image_t = Ext.image_t
	type extern_t = Ext.extern_t

	module C = Compiler.Make (Ext)

	let ambivalent_from_string
		?(verify_utf8 = true)
		?(expand_entities = true)
		?(idiosyncrasies = Idiosyncrasies.default)
		source = Monad.catch
			begin fun () ->
				let () = if verify_utf8 then Preprocessor.verify_utf8 source in
				let ast = Readable.ast_from_string source in
				C.compile ~expand_entities ~idiosyncrasies ~source ast
			end
			begin function
				| Preprocessor.Malformed_source (sane_str, error_lines) ->
					let msgs = List.map (fun line -> (Some line, Error.Malformed_code_point)) error_lines in
					let errors = C.process_errors ~sort:false sane_str msgs in
					Monad.return (Ambivalent.make_invalid errors)
				| Readable.Reading_error (line, msg) ->
					let errors = C.process_errors ~sort:false source [(Some line, Error.Reading_error msg)] in
					Monad.return (Ambivalent.make_invalid errors)
				| exc ->
					Monad.fail exc
			end
end

