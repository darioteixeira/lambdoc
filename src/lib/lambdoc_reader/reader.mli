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

	val ast_from_string: extinldefs:Extcomm.extinldefs_t -> extblkdefs:Extcomm.extblkdefs_t -> string -> Ast.t
end


(**	The signature exported by the functor.
*)
module type S =
sig
	type 'a monad_t
	type linkdata_t
	type imagedata_t
	type extinldata_t
	type extblkdata_t
	type rconfig_t

	val ambivalent_from_string:
		?rconfig:rconfig_t ->
		?verify_utf8:bool ->
		?expand_entities:bool ->
		?idiosyncrasies:Idiosyncrasies.t ->
		string ->
		(linkdata_t, imagedata_t, extinldata_t, extblkdata_t) Ambivalent.t monad_t
end


(**	The signature of a partially applied functor.
*)
module type PARTIAL =
sig
	module Make: functor (Ext: Extension.S) -> S with
		type 'a monad_t = 'a Ext.Monad.t and
		type linkdata_t = Ext.linkdata_t and
		type imagedata_t = Ext.imagedata_t and
		type extinldata_t = Ext.extinldata_t and
		type extblkdata_t = Ext.extblkdata_t and
		type rconfig_t = Ext.rconfig_t
end


(********************************************************************************)
(**	{1 Public modules and functors}						*)
(********************************************************************************)

module Make:
	functor (Readable: READABLE) ->
	functor (Ext: Extension.S) -> S with
		type 'a monad_t = 'a Ext.Monad.t and
		type linkdata_t = Ext.linkdata_t and
		type imagedata_t = Ext.imagedata_t and
		type extinldata_t = Ext.extinldata_t and
		type extblkdata_t = Ext.extblkdata_t and
		type rconfig_t = Ext.rconfig_t

