module Valid = Lambdoc_document_valid
module Invalid = Lambdoc_document_invalid

open Sexplib.Std


(********************************************************************************)
(** {1 Type definitions}                                             		    *)
(********************************************************************************)

type t =
    | Valid of Valid.t
    | Invalid of Invalid.t
    [@@deriving sexp]


(********************************************************************************)
(** {1 Public functions and values}                                      	    *)
(********************************************************************************)

let serialize doc =
    Sexplib.Sexp.to_string_mach (sexp_of_t doc)

let deserialize str =
    t_of_sexp (Sexplib.Sexp.of_string str)

