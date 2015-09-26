(********************************************************************************)
(*  Lambdoc_core_note.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module Block = Lambdoc_core_block
module Label = Lambdoc_core_label
module Order = Lambdoc_core_order


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type order = (Order.ordinal, Order.ordinal Order.auto_given) Order.t with sexp

type t =
    {
    label: Label.t;
    order: order;
    content: Block.frag;
    } with sexp


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let make label order content =
    {label; order; content}

