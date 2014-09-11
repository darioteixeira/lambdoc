(********************************************************************************)
(*	Extension.mli
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Extension.
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

	type link_t
	type image_t
	type extern_t
	type wconfig_t

	val expand_link: ?wconfig:wconfig_t -> Href.t -> link_t -> (Href.t * Inline.seq_t option) Monad.t
	val expand_image: ?wconfig:wconfig_t -> Href.t -> image_t -> Href.t Monad.t
	val expand_extern: ?wconfig:wconfig_t -> Href.t -> extern_t -> Block.frag_t Monad.t
end


(********************************************************************************)
(*	{1 Public modules}							*)
(********************************************************************************)

module Identity: MONAD with type 'a t = 'a

module Unit: S with
	type 'a Monad.t = 'a and
	type link_t = unit and
	type image_t = unit and
	type extern_t = unit and
	type wconfig_t = unit

