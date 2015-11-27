(********************************************************************************)
(*  Lambdoc_core_tabular.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module Attr = Lambdoc_core_attr
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

type colfmt = alignment * weight [@@deriving sexp]

type cellfmt =
    {
    colfmt: colfmt; 
    colspan: int; 
    overline: bool; 
    underline: bool; 
    } [@@deriving sexp]   

type cell =
    {
    attr: Attr.t;
    cellfmt: cellfmt option; 
    seq: Inline.seq option;
    } [@@deriving sexp]

type row = cell list [@@deriving sexp]

type group = row list [@@deriving sexp]

type t =
    {
    tcols: colfmt array option;
    thead: group option;
    tfoot: group option;
    tbodies: group list;
    } [@@deriving sexp]


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let make_cellfmt ~colfmt ~colspan ~overline ~underline = {colfmt; colspan; overline; underline}
let make_cell ?(attr = Attr.default) ?cellfmt seq = {attr; cellfmt; seq}
let make_row row = row
let make_group group = group
let make ?tcols ?thead ?tfoot tbodies = {tcols; thead; tfoot; tbodies}

