(********************************************************************************)
(*  Lambdoc_reader_style.mli
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Parsing of style parameters.
*)

module Ast = Lambdoc_reader_ast

open Lambdoc_core
open Basic


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type _ handle =
    | Lang_hnd: Camlhighlight_core.lang_t option handle
    | Linenums_hnd: bool handle
    | Width_hnd: int option handle

type parsing


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val parse: Ast.command -> classname list * parsing ref * Error.msg list
val consume1: parsing ref -> 'a handle * 'a -> 'a
val consume2: parsing ref -> 'a handle * 'a -> 'b handle * 'b -> 'a * 'b
val dispose: Ast.command -> parsing ref -> Error.msg list

