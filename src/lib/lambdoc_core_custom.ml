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

type kind =
    | Boxout
    | Theorem
    [@@deriving sexp]

type key = pointer [@@deriving sexp]

type order = (Order.ordinal, [ Order.ordinal Order.auto_given | Order.ordinal Order.user_given | Order.none_given ]) Order.t [@@deriving sexp]

type anonymous = [ `Anonymous of key * Label.t ] [@@deriving sexp]

type unnumbered = [ `Unnumbered of key * Label.t ] [@@deriving sexp]

type numbered = [ `Numbered of key * Label.t * (Order.ordinal, [ Order.ordinal Order.auto_given | Order.ordinal Order.user_given | Order.none_given ]) Order.t ] [@@deriving sexp]

type t = [ anonymous | unnumbered | numbered ]


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
    type t = [ anonymous | unnumbered | numbered ] [@@deriving sexp]

    let make x = x
end


module Theorem =
struct
    type t = [ unnumbered | numbered ] [@@deriving sexp]

    let make = function
        | #unnumbered | #numbered as x -> x
        | `Anonymous _                 -> invalid_arg "Lambdoc_core_custom.Theorem.make"
end

