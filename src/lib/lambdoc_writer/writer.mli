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
	type valid_options_t
	type invalid_options_t

	val default_valid_options: valid_options_t
	val default_invalid_options: invalid_options_t

	val from_valid:
		?valid_options:valid_options_t ->
		link_dict:Extension.link_dict_t ->
		image_dict:Extension.image_dict_t ->
		Valid.t ->
		t

	val from_invalid:
		?invalid_options:invalid_options_t ->
		Invalid.t ->
		t
end


(**	The signature exported by the functor.
*)
module type S =
sig
	type t
	type valid_options_t
	type invalid_options_t
	type 'a monad_t
	type link_writer_t
	type image_writer_t

	val default_valid_options: valid_options_t
	val default_invalid_options: invalid_options_t

	val write_valid:
		?valid_options:valid_options_t ->
		?link_writers:link_writer_t list ->
		?image_writers:image_writer_t list ->
		Valid.t ->
		t monad_t

	val write_invalid:
		?invalid_options:invalid_options_t ->
		Invalid.t ->
		t monad_t

	val write_ambivalent:
		?valid_options:valid_options_t ->
		?invalid_options:invalid_options_t ->
		?link_writers:link_writer_t list ->
		?image_writers:image_writer_t list ->
		Ambivalent.t ->
		t monad_t
end


(********************************************************************************)
(**	{1 Modules and functors}						*)
(********************************************************************************)

(** The functor that creates a document writer.
*)
module Make:
	functor (Writable: WRITABLE) ->
	functor (Ext: Extension.S) -> S with
	type t = Writable.t and
	type valid_options_t = Writable.valid_options_t and
	type invalid_options_t = Writable.invalid_options_t and
	type 'a monad_t = 'a Ext.Monad.t and
	type link_writer_t = Ext.link_writer_t and
	type image_writer_t = Ext.image_writer_t

