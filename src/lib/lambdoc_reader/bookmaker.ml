(********************************************************************************)
(*	Bookmaker.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core


(********************************************************************************)
(*	{1 Type definitions}							*)
(********************************************************************************)

type failure_t =
	| Unavailable
	| Uncapable of string
	| Malformed_ISBN of string
	| Unknown_ISBN of string

type result_t =
	| Success of Book.t
	| Failure of failure_t


(********************************************************************************)
(*	{1 Public signatures}							*)
(********************************************************************************)

module type MONAD =
sig
	type 'a t

	val return: 'a -> 'a t
	val fail: exn -> 'a t
	val (>>=): 'a t -> ('a -> 'b t) -> 'b t
	val catch: (unit -> 'a t) -> (exn -> 'a t) -> 'a t
end


module type S =
sig
	module Monad: MONAD

	val resolve: Book.isbn_t list -> (Book.isbn_t * result_t) list Monad.t
end


(********************************************************************************)
(*	{1 Public modules}							*)
(********************************************************************************)

module Identity: MONAD with type 'a t = 'a =
struct
	type 'a t = 'a

	let return x = x
	let fail exc = raise exc
	let (>>=) t f = f t
	let catch f g = try f () with exc -> raise exc
end


module Null =
struct
	module Monad = Identity

	let resolve isbns =
		List.map (fun isbn -> (isbn, Failure Unavailable)) isbns
end

