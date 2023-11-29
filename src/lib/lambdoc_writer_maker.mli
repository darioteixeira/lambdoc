(** Document writer.
*)

open Lambdoc_document


(********************************************************************************)
(** {1 Public signatures}                                                       *)
(********************************************************************************)

(** The module type that all wannabe document writers must export.
*)
module type WRITABLE =
sig
    type t
    type valid_options
    type invalid_options

    val default_valid_options: valid_options
    val default_invalid_options: invalid_options

    val from_valid:
        ?valid_options:valid_options ->
        Valid.t ->
        t

    val from_invalid:
        ?invalid_options:invalid_options ->
        Invalid.t ->
        t
end


(** The signature exported by the functor.
*)
module type WRITER =
sig
    type t
    type valid_options
    type invalid_options

    val default_valid_options: valid_options
    val default_invalid_options: invalid_options

    val write_valid:
        ?valid_options:valid_options ->
        Valid.t ->
        t

    val write_invalid:
        ?invalid_options:invalid_options ->
        Invalid.t ->
        t

    val write_ambivalent:
        ?valid_options:valid_options ->
        ?invalid_options:invalid_options ->
        Ambivalent.t ->
        t
end


(********************************************************************************)
(** {1 Modules and functors}                                                    *)
(********************************************************************************)

(** The functor that creates a document writer.
*)
module Make:
    functor (Writable: WRITABLE) -> WRITER with
    type t = Writable.t and
    type valid_options = Writable.valid_options and
    type invalid_options = Writable.invalid_options

