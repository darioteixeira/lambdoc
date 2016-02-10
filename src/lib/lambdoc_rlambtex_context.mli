(********************************************************************************)
(*  Lambdoc_rlambtex_context.mli
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)


(********************************************************************************)
(** {1 Public signatures}                                                       *)
(********************************************************************************)

module type S =
sig
    type t = Block | Inline | Raw | Mathtexinl | Mathmlinl | Literal

    val push: t -> unit
    val pop: unit -> unit
    val get: unit -> t
end


(********************************************************************************)
(** {1 Public functors}                                                         *)
(********************************************************************************)

module Make: functor (M: sig end) -> S

