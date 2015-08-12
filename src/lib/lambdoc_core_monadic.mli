(********************************************************************************)
(*  Lambdoc_core_monadic.mli
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Monad definition.
*)


(********************************************************************************)
(** {1 Public signatures}                                                       *)
(********************************************************************************)

module type S =
sig
    type 'a t

    val return: 'a -> 'a t
    val fail: exn -> 'a t
    val bind: 'a t -> ('a -> 'b t) -> 'b t
    val catch: (unit -> 'a t) -> (exn -> 'a t) -> 'a t
    val fold_right: ('a -> 'b -> 'b t) -> 'a list -> 'b -> 'b t
end


(********************************************************************************)
(** {1 Public modules}                                                          *)
(********************************************************************************)

module Identity: S with type 'a t = 'a

