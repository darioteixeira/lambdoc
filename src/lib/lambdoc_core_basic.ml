(********************************************************************************)
(*  Lambdoc_core_basic.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Sexplib.Std


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type entity_t = string with sexp
type href_t = string with sexp
type classname_t = string with sexp
type ident_t = string with sexp
type pointer_t = string with sexp

