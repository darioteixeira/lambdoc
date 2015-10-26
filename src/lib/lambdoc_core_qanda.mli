(********************************************************************************)
(*  Lambdoc_core_qanda.mli
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Definitions pertaining to Q&A environments.
*)

module Inline = Lambdoc_core_inline


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type t =
    | New_questioner of Inline.seq option
    | New_answerer of Inline.seq option
    | Same_questioner
    | Same_answerer
    [@@deriving sexp]

