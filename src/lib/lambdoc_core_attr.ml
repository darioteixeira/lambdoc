(********************************************************************************)
(*  Lambdoc_core_attr.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Sexplib.Std
open Lambdoc_core_basic


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type originator =
    | Source
    | Extension
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

let make ?parsinfo classnames =
    {classnames; parsinfo}

let default = make []

