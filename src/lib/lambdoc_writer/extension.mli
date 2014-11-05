(********************************************************************************)
(*	Extension.mli
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Writer extension.
*)

open Lambdoc_core
open Basic


(********************************************************************************)
(*	{1 Public signatures}							*)
(********************************************************************************)

module type MONAD =
sig
	type 'a t

	val return: 'a -> 'a t
	val fail: exn -> 'a t
	val bind: 'a t -> ('a -> 'b t) -> 'b t
	val catch: (unit -> 'a t) -> (exn -> 'a t) -> 'a t
	val iter: ('a -> unit t) -> 'a list -> unit t
end


module type S =
sig
	module Monad: MONAD

	type linkdata_t
	type imagedata_t
	type extinldata_t
	type extblkdata_t
	type wconfig_t

	val write_link: ?wconfig:wconfig_t -> Href.t -> linkdata_t -> (Href.t * Inline.seq_t option) Monad.t
	val write_image: ?wconfig:wconfig_t -> Href.t -> imagedata_t -> Href.t Monad.t
	val write_extinl: ?wconfig:wconfig_t -> Ident.t -> Extcomm.extinl_t -> extinldata_t -> Inline.seq_t Monad.t
	val write_extblk: ?wconfig:wconfig_t -> Ident.t -> Extcomm.extblk_t ->extblkdata_t -> Block.frag_t Monad.t
end


(********************************************************************************)
(*	{1 Public modules}							*)
(********************************************************************************)

module Identity: MONAD with type 'a t = 'a

module Unitary: S with
	type 'a Monad.t = 'a and
	type linkdata_t = unit and
	type imagedata_t = unit and
	type extinldata_t = unit and
	type extblkdata_t = unit and
	type wconfig_t = unit

