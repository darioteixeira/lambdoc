(********************************************************************************)
(*  Lambdoc_core_wrapper.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module Inline = Lambdoc_core_inline
module Label = Lambdoc_core_label
module Order = Lambdoc_core_order

open Sexplib.Std


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type order = (Order.ordinal, [ Order.ordinal Order.auto_given | Order.ordinal Order.user_given | Order.none_given ]) Order.t [@@deriving sexp]

type kind =
    | Printout
    | Equation
    | Figure
    | Table
    [@@deriving sexp]

type t =
    | Ordered of Label.t * (Order.ordinal, [ Order.ordinal Order.auto_given | Order.ordinal Order.user_given ]) Order.t * Inline.seq option
    | Unordered of Label.t * Inline.seq
    [@@deriving sexp]

