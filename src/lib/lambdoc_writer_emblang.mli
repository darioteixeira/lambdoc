(********************************************************************************)
(*  Lambdoc_writer_emblang.mli
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Emblang is a very small DSL whose purpose is to simplify the generation
    of {!Lambdoc_document_valid} values for internal use of the library itself.
*)

open Lambdoc_document.Valid


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val convert: string -> Inline.seq

