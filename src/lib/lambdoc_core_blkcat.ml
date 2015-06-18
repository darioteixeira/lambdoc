(********************************************************************************)
(*  Lambdoc_core_blkcat.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type ranked_t =
    [ `Super_blk
    | `Listable_blk
    | `Quotable_blk
    | `Embeddable_blk
    ] with sexp

type unranked_t =
    [ `Paragraph_blk
    | `Equation_blk
    | `Printout_blk
    | `Table_blk
    | `Figure_blk
    ] with sexp

type t = [ ranked_t | unranked_t ] with sexp


(********************************************************************************)
(** {1 Private functions and values}                                            *)
(********************************************************************************)

let int_of_ranked = function
    | `Super_blk      -> 3
    | `Listable_blk   -> 2
    | `Quotable_blk   -> 1
    | `Embeddable_blk -> 0

let ranked_of_int = function
    | 3 -> `Super_blk
    | 2 -> `Listable_blk
    | 1 -> `Quotable_blk
    | 0 -> `Embeddable_blk
    | _ -> invalid_arg "Blkcat.ranked_of_int"


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let subtype subs sup =
    let is_subtype sub = match (sub, sup) with
        | (x, y) when x = y                        -> true
        | ((#ranked_t as sub), (#ranked_t as sup)) -> int_of_ranked sub <= int_of_ranked sup
        | _                                        -> false in
    List.exists is_subtype subs

let min x1 x2 = match (x1, x2) with
    | ((#ranked_t as x1), (#ranked_t as x2)) -> ranked_of_int (Pervasives.min (int_of_ranked x1) (int_of_ranked x2))
    | _                                      -> invalid_arg "Blkcat.min"

