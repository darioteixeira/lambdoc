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

	type linkdata_t
	type imagedata_t
	type extinldata_t
	type extblkdata_t
	type rconfig_t

	val extinldefs: Extcomm.extinldefs_t
	val extblkdefs: Extcomm.extblkdefs_t

	val read_link: ?rconfig:rconfig_t -> Href.t -> linkdata_t result_t Monad.t
	val read_image: ?rconfig:rconfig_t -> Href.t -> imagedata_t result_t Monad.t
	val read_extinl: ?rconfig:rconfig_t -> Ident.t -> Extcomm.extinl_t -> extinldata_t result_t Monad.t
	val read_extblk: ?rconfig:rconfig_t -> Ident.t -> Extcomm.extblk_t -> extblkdata_t result_t Monad.t
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


module Unitary =
struct
	module Monad = Identity

	type linkdata_t = unit
	type imagedata_t = unit
	type extinldata_t = unit
	type extblkdata_t = unit
	type rconfig_t = unit

	let extinldefs = []
	let extblkdefs = []

	let read_link ?rconfig _ = `Okay ()
	let read_image ?rconfig _ = `Okay ()
	let read_extinl ?rconfig _ _ = `Okay ()
	let read_extblk ?rconfig _ _ = `Okay ()
end

