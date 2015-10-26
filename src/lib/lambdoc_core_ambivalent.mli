(********************************************************************************)
(*  Lambdoc_core_ambivalent.mli
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Definitions concerning ambivalent documents.
    An ambivalent document is one which can either be valid or invalid.
*)

module Invalid = Lambdoc_core_invalid
module Valid = Lambdoc_core_valid


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type t =
    | Valid of Valid.t
    | Invalid of Invalid.t
    [@@deriving sexp]


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

(********************************************************************************)
(** {2 Constructors}                                                            *)
(********************************************************************************)

val valid: Valid.t -> t
val invalid: Invalid.t -> t


(********************************************************************************)
(** {2 Serialisation facilities}                                                *)
(********************************************************************************)

val serialize: t -> string
val deserialize: string -> t

