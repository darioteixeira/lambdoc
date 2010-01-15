(********************************************************************************)
(*	Compiler.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Compilation of a document AST.  These functions convert
	a document AST into a proper, final, ambivalent document.
*)

open Lambdoc_core
open Prelude


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

val process_errors:
	sort:bool ->
	string ->
	(int option * Error.error_msg_t) list ->
	Error.t nelist

val compile_manuscript:
	accept_list: Features.manuscript_feature_t list ->
	deny_list: Features.manuscript_feature_t list ->
	default: Features.default_t ->
	source: string ->
	Ast.t ->
	Ambivalent.manuscript_t

val compile_composition:
	accept_list: Features.composition_feature_t list ->
	deny_list: Features.composition_feature_t list ->
	default: Features.default_t ->
	source: string ->
	Ast.t ->
	Ambivalent.composition_t
