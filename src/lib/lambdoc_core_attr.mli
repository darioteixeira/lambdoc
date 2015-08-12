(********************************************************************************)
(*  Lambdoc_core_attr.mli
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
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

type originator_t =
    | Source        (** Straight from the source as provided by the user *)
    | Extension     (** Synthesised from an extension *)
    with sexp

type parsinfo_t =
    {
    tag: ident_t option;
    linenum: int;
    originator: originator_t;
    } with sexp

type t =
    {
    classnames: classname_t list;
    parsinfo: parsinfo_t option;
    } with sexp


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val make: ?parsinfo:parsinfo_t -> classname_t list -> t
val default: t

