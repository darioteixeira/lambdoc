(********************************************************************************)
(*  Lambdoc_core_blkcat.mli
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Definitions concerning block categories.
*)


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

(** Ranked blocks establish a subtyping hierarchy, where [Super] blocks sit
    at the top (they are the supertype of any block) and [Embeddable] blocks
    sit at the bottom.
*)
type ranked_t =
    [ `Super_blk
    | `Listable_blk
    | `Quotable_blk
    | `Embeddable_blk
    ] with sexp

(** Unranked blocks refer to discrete block categories, and no subtyping
    relation is defined between them.
*)
type unranked_t =
    [ `Paragraph_blk
    | `Equation_blk
    | `Printout_blk
    | `Table_blk
    | `Figure_blk
    ] with sexp

type t = [ ranked_t | unranked_t ] with sexp


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

(** It tells whether any element of the list given as first parameter
    is a subtype of the second.
*)
val subtype: t list -> t -> bool

(** Given two block categories, returns whichever is lowest in the hierarchy.
*)
val min: t -> t -> t

