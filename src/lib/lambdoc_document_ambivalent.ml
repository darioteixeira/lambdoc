(********************************************************************************)
(*  Lambdoc_document_ambivalent.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

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

