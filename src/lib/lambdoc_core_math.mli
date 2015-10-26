(********************************************************************************)
(*  Lambdoc_core_math.mli
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Definitions pertaining to document math.
*)


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type mathtex = string [@@deriving sexp]
type mathml = string [@@deriving sexp]

type t =
    | Mathtex of mathtex
    | Mathml of mathml
    | Both of mathtex * mathml
    [@@deriving sexp]

