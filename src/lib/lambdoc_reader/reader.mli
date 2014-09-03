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

	val ast_from_string: string -> Ast.t
end


(**	The signature exported by the functor.
*)
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


(**	The signature of a partially applied functor.
*)
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

module Make:
	functor (Readable: READABLE) ->
	functor (Ext: Extension.S) -> S with
		type 'a monad_t = 'a Ext.Monad.t and
		type link_t = Ext.link_t and
		type image_t = Ext.image_t and
		type extern_t = Ext.extern_t

