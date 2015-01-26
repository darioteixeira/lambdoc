(********************************************************************************)
(*	Lambdoc_reader_permission.mli
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Document permissions.
*)

open Lambdoc_reader
open Lambdoc_core
open Basic


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

val check_parameters:
	?maybe_minipaged:bool option ->
	?maybe_wrapped:bool option ->
	Ast.command_t ->
	Feature.t ->
	Error.msg_t list

val check_feature:
	Feature.t ->
	Idiosyncrasies.t ->
	bool

val check_classname:
	Feature.t ->
	Classname.t ->
	Idiosyncrasies.t ->
	bool

