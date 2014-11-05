(********************************************************************************)
(*	Writer.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Document writer.
*)

open Lambdoc_core


(********************************************************************************)
(**	{1 Module types}							*)
(********************************************************************************)

(**	The module type that all wannabe document writers must export.
*)
module type WRITABLE =
sig
	type t
	type 'a monad_t
	type linkdata_t
	type imagedata_t
	type extinldata_t
	type extblkdata_t
	type wconfig_t
	type valid_options_t
	type invalid_options_t

	val default_valid_options: valid_options_t
	val default_invalid_options: invalid_options_t

	val write_valid:
		?wconfig:wconfig_t ->
		?valid_options:valid_options_t ->
		(linkdata_t, imagedata_t, extinldata_t, extblkdata_t) Valid.t ->
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
		(linkdata_t, imagedata_t, extinldata_t, extblkdata_t) Ambivalent.t ->
		t monad_t
end


(********************************************************************************)
(**	{1 Modules and functors}						*)
(********************************************************************************)

(** The functor that creates a document writer.
*)
module Make (Writable: WRITABLE): S with
	type t = Writable.t and
	type 'a monad_t = 'a Writable.monad_t and
	type linkdata_t = Writable.linkdata_t and
	type imagedata_t = Writable.imagedata_t and
	type extinldata_t = Writable.extinldata_t and
	type extblkdata_t = Writable.extblkdata_t and
	type wconfig_t = Writable.wconfig_t and
	type valid_options_t = Writable.valid_options_t and
	type invalid_options_t = Writable.invalid_options_t =
struct
	include Writable

	let write_ambivalent ?wconfig ?valid_options ?invalid_options = function
		| Ambivalent.Valid doc   -> write_valid ?wconfig ?valid_options doc
		| Ambivalent.Invalid doc -> write_invalid ?wconfig ?invalid_options doc
end

