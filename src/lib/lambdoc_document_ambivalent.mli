(** Definition of an ambivalent Lambdoc document.
*)

module Valid = Lambdoc_document_valid
module Invalid = Lambdoc_document_invalid


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type t =
    | Valid of Valid.t
    | Invalid of Invalid.t
    [@@deriving sexp]


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val serialize: t -> string
val deserialize: string -> t

