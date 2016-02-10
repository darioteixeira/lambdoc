(********************************************************************************)
(*  Lambdoc_rlambtex_context.ml
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

module Make (M: sig end): S =
struct
    type t = Block | Inline | Raw | Mathtexinl | Mathmlinl | Literal

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

