(********************************************************************************)
(*	Extension.mli
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Reader extension.
*)

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

(**	The signature of the monad that the extension lives under.
*)
module type MONAD =
sig
	type 'a t

	val return: 'a -> 'a t
	val fail: exn -> 'a t
	val bind: 'a t -> ('a -> 'b t) -> 'b t
	val catch: (unit -> 'a t) -> (exn -> 'a t) -> 'a t
	val iter: ('a -> unit t) -> 'a list -> unit t
end


(**	The signature of a reader extension.
*)
module type S =
sig
	module Monad: MONAD

	type linkdata_t		(** Payload associated with {!read_link} *)
	type imagedata_t	(** Payload associated with {!read_image} *)
	type extinldata_t	(** Payload associated with {!read_extinl} *)
	type extblkdata_t	(** Payload associated with {!read_extblk} *)
	type rconfig_t		(** Reader configuration *)

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

module Identity: MONAD with type 'a t = 'a

module Unitary: S with
	type 'a Monad.t = 'a and
	type linkdata_t = unit and
	type imagedata_t = unit and
	type extinldata_t = unit and
	type extblkdata_t = unit and
	type rconfig_t = unit

