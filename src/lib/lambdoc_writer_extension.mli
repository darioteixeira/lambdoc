(********************************************************************************)
(*  Lambdoc_writer_extension.mli
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Writer extension.
*)

open Lambdoc_core
open Basic


(********************************************************************************)
(*  {1 Type definitions}                                                        *)
(********************************************************************************)

type link_dict_t = (Href.t, Href.t) Hashtbl.t

type image_dict_t = (Href.t, Href.t) Hashtbl.t


(********************************************************************************)
(*  {1 Public signatures}                                                       *)
(********************************************************************************)

module type S =
sig
    module Monad: Monadic.S

    type link_writer_t = Href.t -> string option -> Href.t option Monad.t

    type image_writer_t = Href.t -> string option -> Href.t option Monad.t
end


(********************************************************************************)
(*  {1 Public modules}                                                          *)
(********************************************************************************)

module Make: functor (M: Monadic.S) -> S with module Monad = M

module Trivial: S with module Monad = Monadic.Identity

