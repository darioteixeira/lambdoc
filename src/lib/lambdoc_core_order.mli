(********************************************************************************)
(*  Lambdoc_core_order.mli
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Definitions pertaining to document ordering.
*)


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type ordinal = int [@@deriving sexp]

type hierarchical = int list [@@deriving sexp]


(** A block's ordering can be assigned by any of three sources: [`Auto_given] means that
    the ordering should be automatically given by the system; [`User_given] means that the
    ordering is manually given by the user; finally, when the block should not have any
    ordering at all, [`None_given] is used.  Note that different classes of blocks allow
    a different subset of these ordering variants.  Moreover, only the first variant must
    be parametrised over the actual ordering scheme used (as it makes no sense to talk of
    an ordering scheme when [`None_given] is used, for example).
*)

type 'a auto_given = [ `Auto_given of 'a ] [@@deriving sexp]
type 'a user_given = [ `User_given of 'a ] [@@deriving sexp]
type none_given = [ `None_given ] [@@deriving sexp]
type ('a, 'b) t = 'b constraint 'b = [< 'a auto_given | 'a user_given | none_given ] [@@deriving sexp]

