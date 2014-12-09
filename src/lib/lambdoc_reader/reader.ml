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

	val ast_from_string:
		inline_extdefs:Extension.inline_extdef_t list ->
		block_extdefs:Extension.block_extdef_t list ->
		string ->
		Ast.t
end


module type S =
sig
	type 'a monad_t
	type link_reader_t
	type image_reader_t
	type inline_extcomm_t
	type block_extcomm_t

	val ambivalent_from_string:
		?link_readers:link_reader_t list ->
		?image_readers:image_reader_t list ->
		?inline_extcomms:inline_extcomm_t list ->
		?block_extcomms:block_extcomm_t list ->
		?verify_utf8:bool ->
		?expand_entities:bool ->
		?idiosyncrasies:Idiosyncrasies.t ->
		string ->
		Ambivalent.t monad_t
end


module type PARTIAL =
sig
	module Make: functor (Ext: Extension.S) -> S with
		type 'a monad_t = 'a Ext.Monad.t and
		type link_reader_t = Ext.link_reader_t and
		type image_reader_t = Ext.image_reader_t and
		type inline_extcomm_t = Ext.inline_extcomm_t and
		type block_extcomm_t = Ext.block_extcomm_t
end


(********************************************************************************)
(**	{1 Public modules and functors}						*)
(********************************************************************************)

module Make (Readable: READABLE) (Ext: Extension.S) : S with
	type 'a monad_t = 'a Ext.Monad.t and
	type link_reader_t = Ext.link_reader_t and
	type image_reader_t = Ext.image_reader_t and
	type inline_extcomm_t = Ext.inline_extcomm_t and
	type block_extcomm_t = Ext.block_extcomm_t =
struct
	open Ext

	type 'a monad_t = 'a Ext.Monad.t
	type link_reader_t = Ext.link_reader_t
	type image_reader_t = Ext.image_reader_t
	type inline_extcomm_t = Ext.inline_extcomm_t
	type block_extcomm_t = Ext.block_extcomm_t

	module C = Compiler.Make (Ext)

	let inline_extdef_of_extcomm extcomm =
		let open Extension in
		let inlsyn = match extcomm.inlfun with
			| Inlfun_empty _	-> Inlsyn_empty
			| Inlfun_seq _		-> Inlsyn_seq
			| Inlfun_raw _		-> Inlsyn_raw
			| Inlfun_raw_seq _	-> Inlsyn_raw_seq
			| Inlfun_raw_seqopt _	-> Inlsyn_raw_seqopt
			| Inlfun_raw_raw _	-> Inlsyn_raw_raw
		in (extcomm.inltag, inlsyn)

	let block_extdef_of_extcomm extcomm =
		let open Extension in
		let blksyn = match extcomm.blkfun with
			| Blkfun_empty _	-> Blksyn_empty
			| Blkfun_seq _		-> Blksyn_seq
			| Blkfun_raw _		-> Blksyn_raw
			| Blkfun_lit _		-> Blksyn_lit
			| Blkfun_frag _		-> Blksyn_frag
			| Blkfun_raw_raw _	-> Blksyn_raw_raw
		in (extcomm.blktag, blksyn)

	let ambivalent_from_string
		?(link_readers = [])
		?(image_readers = [])
		?(inline_extcomms = [])
		?(block_extcomms = [])
		?(verify_utf8 = true)
		?(expand_entities = true)
		?(idiosyncrasies = Idiosyncrasies.default)
		source =
		Monad.catch
			begin fun () ->
				let () = if verify_utf8 then Preprocessor.verify_utf8 source in
				let inline_extdefs = List.map inline_extdef_of_extcomm inline_extcomms in
				let block_extdefs = List.map block_extdef_of_extcomm block_extcomms in
				let ast = Readable.ast_from_string ~inline_extdefs ~block_extdefs source in
				C.compile ~link_readers ~image_readers ~inline_extcomms ~block_extcomms ~expand_entities ~idiosyncrasies ~source ast
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

