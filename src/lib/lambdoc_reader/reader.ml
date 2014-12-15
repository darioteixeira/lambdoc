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
	type extcomm_t

	val ambivalent_from_string:
		?link_readers:link_reader_t list ->
		?image_readers:image_reader_t list ->
		?extcomms:extcomm_t list ->
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
		type extcomm_t = Ext.extcomm_t
end


(********************************************************************************)
(**	{1 Public modules and functors}						*)
(********************************************************************************)

module Make (Readable: READABLE) (Ext: Extension.S) : S with
	type 'a monad_t = 'a Ext.Monad.t and
	type link_reader_t = Ext.link_reader_t and
	type image_reader_t = Ext.image_reader_t and
	type extcomm_t = Ext.extcomm_t =
struct
	open Ext
	open Extension

	type 'a monad_t = 'a Ext.Monad.t
	type link_reader_t = Ext.link_reader_t
	type image_reader_t = Ext.image_reader_t
	type extcomm_t = Ext.extcomm_t

	module C = Compiler.Make (Ext)

	let extdef_of_extcomm (accum_inl, accum_blk) (tag, def) = match def with
		| Inlextcomm inlfun ->
			let inlsyn = match inlfun with
				| Inlfun_empty _	-> Inlsyn_empty
				| Inlfun_seq _		-> Inlsyn_seq
				| Inlfun_raw _		-> Inlsyn_raw
				| Inlfun_raw_seq _	-> Inlsyn_raw_seq
				| Inlfun_raw_seqopt _	-> Inlsyn_raw_seqopt
				| Inlfun_raw_raw _	-> Inlsyn_raw_raw
			in ((tag, inlsyn) :: accum_inl, accum_blk)
		| Blkextcomm (blkfun, _) ->
			let blksyn = match blkfun with
				| Blkfun_empty _	-> Blksyn_empty
				| Blkfun_seq _		-> Blksyn_seq
				| Blkfun_raw _		-> Blksyn_raw
				| Blkfun_lit _		-> Blksyn_lit
				| Blkfun_frag _		-> Blksyn_frag
				| Blkfun_raw_raw _	-> Blksyn_raw_raw
			in (accum_inl, (tag, blksyn) :: accum_blk)

	let ambivalent_from_string
		?(link_readers = [])
		?(image_readers = [])
		?(extcomms = [])
		?(verify_utf8 = true)
		?(expand_entities = true)
		?(idiosyncrasies = Idiosyncrasies.default)
		source =
		Monad.catch
			begin fun () ->
				let () = if verify_utf8 then Preprocessor.verify_utf8 source in
				let (inline_extdefs, block_extdefs) = List.fold_left extdef_of_extcomm ([], []) extcomms in
				let ast = Readable.ast_from_string ~inline_extdefs ~block_extdefs source in
				C.compile ~link_readers ~image_readers ~extcomms ~expand_entities ~idiosyncrasies ~source ast
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

