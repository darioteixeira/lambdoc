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

module Make (M: sig end): S =
struct
    type t = Block | Inline | Raw | Mathtex_inl | Literal

    let stack = ref []

    let push context =
        stack := context :: !stack

    let pop () = match !stack with
        | _ :: tl -> stack := tl
        | []      -> assert false

    let get () = match !stack with
        | hd :: _ -> hd
        | []      -> Block
end

