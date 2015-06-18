(********************************************************************************)
(*  Lambdoc_core_qanda.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module Inline = Lambdoc_core_inline

open Sexplib.Std


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type t =
    | New_questioner of Inline.seq_t option
    | New_answerer of Inline.seq_t option
    | Same_questioner
    | Same_answerer
    with sexp

