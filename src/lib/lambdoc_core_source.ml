(********************************************************************************)
(*  Lambdoc_core_source.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Sexplib.Std


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type t =
    {
    lang: Camlhighlight_core.lang option;
    hilite: Camlhighlight_core.t;
    linenums: bool;
    } with sexp


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let make lang hilite linenums =
    {lang; hilite; linenums}

