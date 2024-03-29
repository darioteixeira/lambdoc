(** Definitions concerning block categories.
*)


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

(** Ranked blocks establish a subtyping hierarchy, where [Super] blocks sit
    at the top (they are the supertype of any block) and [Embeddable] blocks
    sit at the bottom.
*)
type ranked =
    [ `Super_blk
    | `Listable_blk
    | `Quotable_blk
    | `Embeddable_blk
    ] [@@deriving sexp]

(** Unranked blocks refer to discrete block categories, and no subtyping
    relation is defined between them.
*)
type unranked =
    [ `Paragraph_blk
    | `Equation_blk
    | `Printout_blk
    | `Table_blk
    | `Figure_blk
    ] [@@deriving sexp]

type t = [ ranked | unranked ] [@@deriving sexp]


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

