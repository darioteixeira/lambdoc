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
val parse_for_pullquote: 	(int * Error.error_msg_t) DynArray.t -> Ast.command_t -> Floatation.t
val parse_for_boxout: 		?classnames:string list -> (int * Error.error_msg_t) DynArray.t -> Ast.command_t -> Floatation.t * string option
val parse_for_mathtex: 		(int * Error.error_msg_t) DynArray.t -> Ast.command_t -> Floatation.t
val parse_for_mathml: 		(int * Error.error_msg_t) DynArray.t -> Ast.command_t -> Floatation.t
val parse_for_program:		(int * Error.error_msg_t) DynArray.t -> Ast.command_t -> Floatation.t * Camlhighlight_core.lang_t option * bool * bool
val parse_for_tabular:		(int * Error.error_msg_t) DynArray.t -> Ast.command_t -> Floatation.t
val parse_for_verbatim:		(int * Error.error_msg_t) DynArray.t -> Ast.command_t -> Floatation.t
val parse_for_bitmap:		(int * Error.error_msg_t) DynArray.t -> Ast.command_t -> Floatation.t * bool * int option
val parse_for_subpage:		(int * Error.error_msg_t) DynArray.t -> Ast.command_t -> Floatation.t
val parse_for_macrodef:		(int * Error.error_msg_t) DynArray.t -> Ast.command_t -> int

