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

(**	The signature that all wannabe document readers must export.
*)
module type READABLE =
sig
	exception Reading_error of int * string

	val ast_from_string:
		inline_extdefs:Extension.inline_extdef_t list ->
		block_extdefs:Extension.block_extdef_t list ->
		string ->
		Ast.t
end


(**	The signature exported by the functor.
*)
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


(**	The signature of a partially applied functor.
*)
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

module Make:
	functor (Readable: READABLE) ->
	functor (Ext: Extension.S) -> S with
		type 'a monad_t = 'a Ext.Monad.t and
		type link_reader_t = Ext.link_reader_t and
		type image_reader_t = Ext.image_reader_t and
		type inline_extcomm_t = Ext.inline_extcomm_t and
		type block_extcomm_t = Ext.block_extcomm_t

