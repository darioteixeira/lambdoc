(********************************************************************************)
(*	Macromap.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	The macro map contains a mapping between the labels used to identify
	document macros and their respective  definition.  Note that macro
	labels share the same namespace as other labels.  Users are therefore
	encouraged to use a namespacing convention.
*)

open Lambdoc_core.Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type key_t = raw_t
type value_t = int * Ast.seq_t
type t


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

val create: unit -> t
val add: t -> key_t -> value_t -> unit
val mem: t -> key_t -> bool
val find: t -> key_t -> value_t

