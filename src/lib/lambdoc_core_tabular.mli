(********************************************************************************)
(*  Lambdoc_core_tabular.mli
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Definitions concerning tabular environments.
*)

module Inline = Lambdoc_core_inline


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

type cellspec = colspec * int * bool * bool [@@deriving sexp]   (* column spec, column span, has overline, has underline *)

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

val make_cell: cellspec option -> Inline.seq option -> cell
val make_row: cell list -> row
val make_group: row list -> group
val make: colspec array -> ?thead:group -> ?tfoot:group -> group list -> t

