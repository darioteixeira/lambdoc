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
(**	{1 Type definitions}							*)
(********************************************************************************)

type error_t = [ `Unsupported | `Failed of string | `Style of string ]
type 'a result_t = [ `Okay of 'a | `Error of error_t ]


(********************************************************************************)
(**	{1 Public signatures}							*)
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
	type rconfig_t

	val resolve_link: Href.t -> string option -> rconfig_t option -> link_t result_t Monad.t
	val resolve_image: Href.t -> string option -> rconfig_t option -> image_t result_t Monad.t
	val resolve_extern: Href.t -> string option -> rconfig_t option -> extern_t result_t Monad.t
end


(********************************************************************************)
(**	{1 Public modules}							*)
(********************************************************************************)

module Identity =
struct
	type 'a t = 'a

	let return x = x
	let fail exc = raise exc
	let bind t f = f t
	let catch f g = try f () with exc -> g exc
	let iter = List.iter
end


module Unit =
struct
	module Monad = Identity

	type link_t = unit
	type image_t = unit
	type extern_t = unit
	type rconfig_t = unit

	let resolve_link _ _ _ = `Okay ()
	let resolve_image _ _ _ = `Okay ()
	let resolve_extern _ _ _ = `Okay ()
end

