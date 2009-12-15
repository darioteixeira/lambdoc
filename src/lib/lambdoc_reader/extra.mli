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
(**	{1 Type definitions}							*)
(********************************************************************************)

type handle_t =
	| Initial_hnd
	| Linenums_hnd
	| Zebra_hnd
	| Frame_hnd
	| Width_hnd
	| Bullet_hnd
	| Numbering_hnd
	| Floatation_hnd
	| Lang_hnd
	| Args_hnd

type error_t = (int * Error.error_msg_t) DynArray.t

type extra_t


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Parsing multiple handles}						*)
(********************************************************************************)

val parse: Ast.command_t -> error_t -> handle_t list -> extra_t

val get_boolean: default:bool -> extra_t -> handle_t -> bool
val get_numeric: default:int-> extra_t -> handle_t -> int
val get_maybenum: default:int option -> extra_t -> handle_t -> int option
val get_bullet: default:Bullet.t -> extra_t -> handle_t -> Bullet.t
val get_numbering: default:Numbering.t -> extra_t -> handle_t -> Numbering.t
val get_floatation: default:Floatation.t -> extra_t -> handle_t -> Floatation.t
val get_lang: default:Camlhighlight_core.lang_t option -> extra_t -> handle_t -> Camlhighlight_core.lang_t option


(********************************************************************************)
(**	{2 Parsing a single handle}						*)
(********************************************************************************)

val fetch_boolean: default:bool -> Ast.command_t -> error_t -> handle_t -> bool
val fetch_numeric: default:int -> Ast.command_t -> error_t -> handle_t -> int
val fetch_maybenum: default:int option -> Ast.command_t -> error_t -> handle_t -> int option
val fetch_bullet: default:Bullet.t -> Ast.command_t -> error_t -> handle_t -> Bullet.t
val fetch_numbering: default:Numbering.t -> Ast.command_t -> error_t -> handle_t -> Numbering.t
val fetch_floatation: default:Floatation.t -> Ast.command_t -> error_t -> handle_t -> Floatation.t
val fetch_lang: default:Camlhighlight_core.lang_t option -> Ast.command_t -> error_t -> handle_t -> Camlhighlight_core.lang_t option

