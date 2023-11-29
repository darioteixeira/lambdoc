(** Explains errors.
*)

open Lambdoc_document
open Valid
open Invalid


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val explain: Error.contextualized -> Inline.seq

