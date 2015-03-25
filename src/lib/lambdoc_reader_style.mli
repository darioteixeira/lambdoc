(********************************************************************************)
(*	Lambdoc_reader_style.mli
	Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Parsing of style parameters.
*)

module Ast = Lambdoc_reader_ast

open Lambdoc_core
open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type _ handle_t =
	| Lang_hnd: Camlhighlight_core.lang_t option handle_t
	| Linenums_hnd: bool handle_t
	| Width_hnd: int option handle_t

type parsing_t


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

val parse: Ast.command_t -> Attr.t * parsing_t ref * Error.msg_t list
val consume1: parsing_t ref -> 'a handle_t * 'a -> 'a
val consume2: parsing_t ref -> 'a handle_t * 'a -> 'b handle_t * 'b -> 'a * 'b
val dispose: Ast.command_t -> parsing_t ref -> Error.msg_t list

