(********************************************************************************)
(*  Lambdoc_core_custom.mli
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Custom document blocks.
*)

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
    with sexp

type key = pointer with sexp

type order = (Order.ordinal, [ Order.ordinal Order.auto_given | Order.ordinal Order.user_given | Order.none_given ]) Order.t with sexp

type anonymous = [ `Anonymous of key * Label.t ] with sexp

type unnumbered = [ `Unnumbered of key * Label.t ] with sexp

type numbered = [ `Numbered of key * Label.t * (Order.ordinal, [ Order.ordinal Order.auto_given | Order.ordinal Order.user_given | Order.none_given ]) Order.t ] with sexp

type t = [ anonymous | unnumbered | numbered ]


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val anonymous: key -> Label.t -> order -> [> anonymous ]
val unnumbered: key -> Label.t -> order -> [> unnumbered ]
val numbered: key -> Label.t -> order -> [> numbered ]


(********************************************************************************)
(** {1 Public modules}                                                          *)
(********************************************************************************)

module Boxout:
sig
    type t = [ anonymous | unnumbered | numbered ] with sexp

    val make: [ anonymous | unnumbered | numbered ] -> t
end


module Theorem:
sig
    type t = [ unnumbered | numbered ] with sexp

    val make: [ anonymous | unnumbered | numbered ] -> t
end

