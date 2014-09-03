(********************************************************************************)
(*	Extension.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

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

	val expand_link: Href.t * link_t -> (Href.t * Inline.seq_t option) Monad.t
	val expand_image: Href.t * image_t -> Href.t Monad.t
	val expand_extern: Href.t * extern_t -> Block.frag_t Monad.t
end


(********************************************************************************)
(*	{1 Public modules}							*)
(********************************************************************************)

module Identity =
struct
	type 'a t = 'a

	let return x = x
	let fail exc = raise exc
	let bind t f = f t
	let catch f g = try f () with exc -> raise exc
	let iter = List.iter
end


module Unit =
struct
	module Monad = Identity

	type link_t = unit
	type image_t = unit
	type extern_t = unit

	let expand_link (href, _) = (href, None)
	let expand_image (href, _) = href
	let expand_extern _ = []
end

