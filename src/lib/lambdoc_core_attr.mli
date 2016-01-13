(********************************************************************************)
(*  Lambdoc_core_attr.mli
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Attributes attached to inline and block elements.
*)

open Lambdoc_core_basic


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type originator =
    | Source        (** Straight from the source as provided by the user *)
    | Extension     (** Synthesised from an extension *)
    [@@deriving sexp]

type parsinfo =
    {
    tag: ident option;
    linenum: int;
    originator: originator;
    } [@@deriving sexp]

type t =
    {
    classnames: classname list;
    parsinfo: parsinfo option;
    } [@@deriving sexp]


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val make: ?parsinfo:parsinfo -> classname list -> t
val default: t

