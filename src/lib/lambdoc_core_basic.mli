(********************************************************************************)
(*  Lambdoc_core_basic.mli
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Definition of the basic data types used in documents.
*)


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type entity = string [@@deriving sexp]      (** HTML entities *)
type href = string [@@deriving sexp]        (** References to external resources *)
type classname = string [@@deriving sexp]   (** CSS classnames *)
type ident = string [@@deriving sexp]       (** Identifiers *)
type pointer = string [@@deriving sexp]     (** Internal references *)

