(********************************************************************************)
(*  Lambdoc_rlambtex_context.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)


(********************************************************************************)
(** {1 Public signatures}                                                       *)
(********************************************************************************)

module type S =
sig
    type t = General | Raw | Mathtexinl | Mathmlinl | Literal

    val set: t -> unit
    val get: unit -> t
end


(********************************************************************************)
(** {1 Public functors}                                                         *)
(********************************************************************************)

module Make (M: sig end): S =
struct
    type t = General | Raw | Mathtexinl | Mathmlinl | Literal

    let to_string = function
        | General    -> "General"
        | Raw        -> "Raw"
        | Mathtexinl -> "Mathtexinl"
        | Mathmlinl  -> "Mathmlinl"
        | Literal    -> "Literal"

    let cur = ref General

    let set context =
        cur := context

    let get () =
        !cur
end

