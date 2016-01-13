(********************************************************************************)
(*  Lambdoc_core_order.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Sexplib.Std


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type ordinal = int [@@deriving sexp]

type hierarchical = int list [@@deriving sexp]

type 'a auto_given = [ `Auto_given of 'a ] [@@deriving sexp]
type 'a user_given = [ `User_given of 'a ] [@@deriving sexp]
type none_given = [ `None_given ] [@@deriving sexp]
type ('a, 'b) t = 'b constraint 'b = [< 'a auto_given | 'a user_given | none_given ] [@@deriving sexp]

