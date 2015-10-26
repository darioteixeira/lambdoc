(********************************************************************************)
(*  Lambdoc_core_note.mli
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Definitions concerning note blocks.
*)

module Block = Lambdoc_core_block
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
    content: Block.frag;
    } [@@deriving sexp]


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val make: Label.t -> order -> Block.frag -> t

