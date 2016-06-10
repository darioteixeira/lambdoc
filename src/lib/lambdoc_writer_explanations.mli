(********************************************************************************)
(*  Lambdoc_writer_explanations.mli
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Explains errors.
*)

open Lambdoc_document
open Valid
open Invalid


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val explain: Error.contextualized -> Inline.seq

