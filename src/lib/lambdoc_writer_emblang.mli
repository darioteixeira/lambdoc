(** Emblang is a very small DSL whose purpose is to simplify the generation
    of {!Lambdoc_document_valid} values for internal use of the library itself.
*)

open Lambdoc_document.Valid


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val convert: string -> Inline.seq

