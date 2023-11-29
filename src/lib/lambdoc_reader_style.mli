(** Parsing of style parameters.
*)

module Ast = Lambdoc_reader_ast

open Lambdoc_document
open Valid
open Invalid


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type _ handle =
    | Lang_hnd: Camlhighlight_core.lang option handle
    | Linenums_hnd: bool handle
    | Width_hnd: int option handle
    | Cols_hnd: Tabular.colfmt array option handle
    | Cell_hnd: Tabular.cellfmt option handle

type parsing


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val parse: Ast.command -> classname list * parsing ref * Error.msg list
val consume1: parsing ref -> 'a handle * 'a -> 'a
val consume2: parsing ref -> 'a handle * 'a -> 'b handle * 'b -> 'a * 'b
val dispose: Ast.command -> parsing ref -> Error.msg list

