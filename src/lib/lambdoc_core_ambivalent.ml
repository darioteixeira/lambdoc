(********************************************************************************)
(*  Lambdoc_core_ambivalent.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module Invalid = Lambdoc_core_invalid
module Valid = Lambdoc_core_valid


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type t =
    | Valid of Valid.t
    | Invalid of Invalid.t
    with sexp


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

(********************************************************************************)
(** {2 Constructors}                                                            *)
(********************************************************************************)

let valid doc =
    Valid doc

let invalid errors =
    Invalid errors


(********************************************************************************)
(** {2 Serialisation facilities}                                                *)
(********************************************************************************)

let serialize doc =
    Sexplib.Sexp.to_string_mach (sexp_of_t doc)

let deserialize str =
    t_of_sexp (Sexplib.Sexp.of_string str)

