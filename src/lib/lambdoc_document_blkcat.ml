open Sexplib.Std


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type ranked =
    [ `Super_blk
    | `Listable_blk
    | `Quotable_blk
    | `Embeddable_blk
    ] [@@deriving sexp]

type unranked =
    [ `Paragraph_blk
    | `Equation_blk
    | `Printout_blk
    | `Table_blk
    | `Figure_blk
    ] [@@deriving sexp]

type t = [ ranked | unranked ] [@@deriving sexp]


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
    | _ -> assert false


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let subtype subs sup =
    let is_subtype sub = match (sub, sup) with
        | (x, y) when x = y                    -> true
        | ((#ranked as sub), (#ranked as sup)) -> int_of_ranked sub <= int_of_ranked sup
        | _                                    -> false in
    List.exists is_subtype subs

let min x1 x2 = match (x1, x2) with
    | ((#ranked as x1), (#ranked as x2)) -> ranked_of_int (Pervasives.min (int_of_ranked x1) (int_of_ranked x2))
    | _                                  -> invalid_arg "Lambdoc_document_blkcat.min"

