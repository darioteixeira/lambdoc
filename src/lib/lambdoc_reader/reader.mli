(********************************************************************************)
(*	Reader.mli
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Document reader.
*)

open Lambdoc_core


(********************************************************************************)
(**	{1 Public signatures}							*)
(********************************************************************************)

(**	The signature that all wannabe document readers must implement.
*)
module type READABLE =
sig
	exception Reading_error of int * string

	val ast_from_string:
		linenum_offset:int ->
		inline_extdefs:Extension.inline_extdef_t list ->
		block_extdefs:Extension.block_extdef_t list ->
		string ->
		Ast.t
end


(**	The signature of a document reader.
*)
module type READER =
sig
	type 'a monad_t
	type link_reader_t
	type image_reader_t
	type extcomm_t

	val ambivalent_from_string:
		?linenum_offset:int ->
		?link_readers:link_reader_t list ->
		?image_readers:image_reader_t list ->
		?extcomms:extcomm_t list ->
		?verify_utf8:bool ->
		?expand_entities:bool ->
		?idiosyncrasies:Idiosyncrasies.t ->
		string ->
		Ambivalent.t monad_t
end


(**	The complete signature for a reader with ancillary modules.
*)
module type FULL =
sig
	module Readable: READABLE

	module Make: functor (Ext: Extension.S) -> READER with
		type 'a monad_t = 'a Ext.Monad.t and
		type link_reader_t = Ext.link_reader_t and
		type image_reader_t = Ext.image_reader_t and
		type extcomm_t = Ext.extcomm_t

	module Trivial: READER with
		type 'a monad_t = 'a Extension.Trivial.Monad.t and
		type link_reader_t = Extension.Trivial.link_reader_t and
		type image_reader_t = Extension.Trivial.image_reader_t and
		type extcomm_t = Extension.Trivial.extcomm_t
end


(********************************************************************************)
(**	{1 Public modules and functors}						*)
(********************************************************************************)

module Make:
	functor (Readable: READABLE) ->
	functor (Ext: Extension.S) -> READER with
		type 'a monad_t = 'a Ext.Monad.t and
		type link_reader_t = Ext.link_reader_t and
		type image_reader_t = Ext.image_reader_t and
		type extcomm_t = Ext.extcomm_t

