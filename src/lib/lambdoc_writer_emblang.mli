(********************************************************************************)
(*  Lambdoc_writer_emblang.mli
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Emblang is a very small DSL whose purpose is to simplify the generation
    of Lambdoc_core values for internal use of the library itself.
*)


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val convert: string -> Lambdoc_core.Inline.seq_t

