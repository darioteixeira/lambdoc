(********************************************************************************)
(*	Extra.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of the extra parameters.
*)

open Lambdoc_core


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

val parse_for_paragraph: 	(int * Error.error_msg_t) DynArray.t -> Ast.command_t -> bool
val parse_for_itemize: 		(int * Error.error_msg_t) DynArray.t -> Ast.command_t -> Bullet.t
val parse_for_enumerate:	(int * Error.error_msg_t) DynArray.t -> Ast.command_t -> Numbering.t
val parse_for_source:		(int * Error.error_msg_t) DynArray.t -> Ast.command_t -> Camlhighlight_core.lang_t option * bool * bool
val parse_for_image:		(int * Error.error_msg_t) DynArray.t -> Ast.command_t -> bool * int option
val parse_for_decor: 		(int * Error.error_msg_t) DynArray.t -> Ast.command_t -> Floatation.t
val parse_for_pullquote: 	(int * Error.error_msg_t) DynArray.t -> Ast.command_t -> Floatation.t
val parse_for_custom: 		(int * Error.error_msg_t) DynArray.t -> Ast.command_t -> Floatation.t
val parse_for_wrapper: 		(int * Error.error_msg_t) DynArray.t -> Ast.command_t -> Floatation.t
val parse_for_macrodef:		(int * Error.error_msg_t) DynArray.t -> Ast.command_t -> int
val parse_for_customdef:	envname:string -> (int * Error.error_msg_t) DynArray.t -> Ast.command_t -> string

