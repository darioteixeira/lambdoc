(********************************************************************************)
(*	Writer.mli
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Document writer.
*)

open Lambdoc_core


(********************************************************************************)
(**	{1 Public signatures}							*)
(********************************************************************************)

(**	The module type that all wannabe document writers must export.
*)
module type WRITABLE =
sig
	type t
	type 'a monad_t
	type link_t
	type image_t
	type extern_t
	type wconfig_t
	type valid_options_t
	type invalid_options_t

	val default_valid_options: valid_options_t
	val default_invalid_options: invalid_options_t

	val write_valid:
		?wconfig:wconfig_t ->
		?valid_options:valid_options_t ->
		(link_t, image_t, extern_t) Valid.t ->
		t monad_t

	val write_invalid:
		?wconfig:wconfig_t ->
		?invalid_options:invalid_options_t ->
		Invalid.t ->
		t monad_t
end


(**	The signature exported by the functor.
*)
module type S =
sig
	include WRITABLE

	val write_ambivalent:
		?wconfig:wconfig_t ->
		?valid_options:valid_options_t ->
		?invalid_options:invalid_options_t ->
		(link_t, image_t, extern_t) Ambivalent.t ->
		t monad_t
end


(********************************************************************************)
(**	{1 Modules and functors}						*)
(********************************************************************************)

(** The functor that creates a document writer.
*)
module Make: functor (Writable: WRITABLE) -> S with
	type t = Writable.t and
	type 'a monad_t = 'a Writable.monad_t and
	type link_t = Writable.link_t and
	type image_t = Writable.image_t and
	type extern_t = Writable.extern_t and
	type wconfig_t = Writable.wconfig_t and
	type valid_options_t = Writable.valid_options_t and
	type invalid_options_t = Writable.invalid_options_t

