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
    with sexp

type weight =
    | Normal
    | Strong
    with sexp

type colspec = alignment * weight with sexp

type cellspec = colspec * int * bool * bool with sexp

type cell = cellspec option * Inline.seq option with sexp

type row = cell list with sexp

type group = row list with sexp

type t =
    {
    tcols: colspec array;
    thead: group option;
    tfoot: group option;
    tbodies: group list;
    } with sexp


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let make_cell cellspec maybe_seq = (cellspec, maybe_seq)
let make_row row = row
let make_group group = group
let make tcols ?thead ?tfoot tbodies = {tcols; thead; tfoot; tbodies}

