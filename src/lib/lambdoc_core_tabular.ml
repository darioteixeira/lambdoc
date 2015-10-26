(********************************************************************************)
(*  Lambdoc_core_tabular.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module Inline = Lambdoc_core_inline

open Sexplib.Std


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type alignment =
    | Center
    | Left
    | Right
    | Justify
    [@@deriving sexp]

type weight =
    | Normal
    | Strong
    [@@deriving sexp]

type colspec = alignment * weight [@@deriving sexp]

type cellspec = colspec * int * bool * bool [@@deriving sexp]

type cell = cellspec option * Inline.seq option [@@deriving sexp]

type row = cell list [@@deriving sexp]

type group = row list [@@deriving sexp]

type t =
    {
    tcols: colspec array;
    thead: group option;
    tfoot: group option;
    tbodies: group list;
    } [@@deriving sexp]


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let make_cell cellspec maybe_seq = (cellspec, maybe_seq)
let make_row row = row
let make_group group = group
let make tcols ?thead ?tfoot tbodies = {tcols; thead; tfoot; tbodies}

