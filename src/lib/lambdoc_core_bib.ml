(********************************************************************************)
(*  Lambdoc_core_bib.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module Inline = Lambdoc_core_inline
module Label = Lambdoc_core_label
module Order = Lambdoc_core_order


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type order = (Order.ordinal, Order.ordinal Order.auto_given) Order.t [@@deriving sexp]

type t =
    {
    label: Label.t;
    order: order;
    author: Inline.seq;
    title: Inline.seq;
    resource: Inline.seq;
    } [@@deriving sexp]


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let make ~label ~order ~author ~title ~resource =
    {label; order; author; title; resource}

