(********************************************************************************)
(*  Lambdoc_core_custom.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module Basic = Lambdoc_core_basic
module Label = Lambdoc_core_label
module Order = Lambdoc_core_order

open Basic


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type kind_t =
    | Boxout
    | Theorem
    with sexp

type key_t = pointer_t with sexp

type order_t = (Order.ordinal_t, [ Order.ordinal_t Order.auto_given_t | Order.ordinal_t Order.user_given_t | Order.none_given_t ]) Order.t with sexp

type anonymous_t = [ `Anonymous of key_t * Label.t ] with sexp

type unnumbered_t = [ `Unnumbered of key_t * Label.t ] with sexp

type numbered_t = [ `Numbered of key_t * Label.t * (Order.ordinal_t, [ Order.ordinal_t Order.auto_given_t | Order.ordinal_t Order.user_given_t | Order.none_given_t ]) Order.t ] with sexp

type t = [ anonymous_t | unnumbered_t | numbered_t ]


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let anonymous key label = function
    | `None_given -> `Anonymous (key, label)
    | _           -> invalid_arg "Lambdoc_core_custom.anonymous"

let unnumbered key label = function
    | `None_given -> `Unnumbered (key, label)
    | _           -> invalid_arg "Lambdoc_core_custom.unnumbered"

let numbered key label order =
    `Numbered (key, label, order)


(********************************************************************************)
(** {1 Public modules}                                                          *)
(********************************************************************************)

module Boxout =
struct
    type t = [ anonymous_t | unnumbered_t | numbered_t ] with sexp

    let make x = x
end


module Theorem =
struct
    type t = [ unnumbered_t | numbered_t ] with sexp

    let make = function
        | #unnumbered_t | #numbered_t as x -> x
        | `Anonymous _                     -> invalid_arg "Lambdoc_core_custom.Theorem.make"
end

