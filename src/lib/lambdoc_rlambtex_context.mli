(********************************************************************************)
(** {1 Public signatures}                                                       *)
(********************************************************************************)

module type S =
sig
    type t = Block | Inline | Raw | Mathtex_inl | Literal

    val push: t -> unit
    val pop: unit -> unit
    val get: unit -> t
end


(********************************************************************************)
(** {1 Public functors}                                                         *)
(********************************************************************************)

module Make: functor (M: sig end) -> S

