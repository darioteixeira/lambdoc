(********************************************************************************)
(*  Lambdoc_writer_maker.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Document writer.
*)

open Lambdoc_core


(********************************************************************************)
(** {1 Public signatures}                                                       *)
(********************************************************************************)

(** The module type that all wannabe document writers must export.
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
        Valid.t ->
        t

    val from_invalid:
        ?invalid_options:invalid_options_t ->
        Invalid.t ->
        t
end


(** The signature exported by the functor.
*)
module type WRITER =
sig
    type t
    type valid_options_t
    type invalid_options_t

    val default_valid_options: valid_options_t
    val default_invalid_options: invalid_options_t

    val write_valid:
        ?valid_options:valid_options_t ->
        Valid.t ->
        t

    val write_invalid:
        ?invalid_options:invalid_options_t ->
        Invalid.t ->
        t

    val write_ambivalent:
        ?valid_options:valid_options_t ->
        ?invalid_options:invalid_options_t ->
        Ambivalent.t ->
        t
end


(********************************************************************************)
(** {1 Modules and functors}                                                    *)
(********************************************************************************)

(** The functor that creates a document writer.
*)
module Make (Writable: WRITABLE): WRITER with
    type t = Writable.t and
    type valid_options_t = Writable.valid_options_t and
    type invalid_options_t = Writable.invalid_options_t =
struct
    type t = Writable.t
    type valid_options_t = Writable.valid_options_t
    type invalid_options_t = Writable.invalid_options_t

    let default_valid_options = Writable.default_valid_options
    let default_invalid_options = Writable.default_invalid_options

    let write_valid ?valid_options doc =
        Writable.from_valid ?valid_options doc

    let write_invalid ?invalid_options doc =
        Writable.from_invalid ?invalid_options doc

    let write_ambivalent ?valid_options ?invalid_options = function
        | Ambivalent.Valid doc   -> write_valid ?valid_options doc
        | Ambivalent.Invalid doc -> write_invalid ?invalid_options doc
end

