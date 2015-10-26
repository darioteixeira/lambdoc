(********************************************************************************)
(*  Lambdoc_core_level.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Sexplib.Std


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type section = int [@@deriving sexp]

type title = int [@@deriving sexp]


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let max_section = 6

let max_title = 2

let section level =
    if level >= 1 && level <= max_section
    then level
    else invalid_arg ("Lambdoc_core_level.section: " ^ string_of_int level)

let title level =
    if level >= 1 && level <= max_title
    then level
    else invalid_arg ("Lambdoc_core_level.title: " ^ string_of_int level)

