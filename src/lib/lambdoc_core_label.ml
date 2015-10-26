(********************************************************************************)
(*  Lambdoc_core_label.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core_basic


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type t =
    | Auto of pointer
    | User of pointer
    [@@deriving sexp]


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let compare = Pervasives.compare

