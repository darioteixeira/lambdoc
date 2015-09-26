(********************************************************************************)
(*  Lambdoc_core_invalid.mli
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Definitions concerning invalid documents.
*)

module Error = Lambdoc_core_error


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type t = Error.contextualized list with sexp


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

(********************************************************************************)
(** {2 Constructors}                                                            *)
(********************************************************************************)

val make: Error.contextualized list -> t


(********************************************************************************)
(** {2 Serialisation facilities}                                                *)
(********************************************************************************)

val serialize: t -> string
val deserialize: string -> t

