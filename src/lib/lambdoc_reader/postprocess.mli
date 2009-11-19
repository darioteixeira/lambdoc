(********************************************************************************)
(*	Postprocess.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Postprocessing on a document AST.  These functions convert
	a document AST into a proper, final, ambivalent document.
*)

open Lambdoc_core


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

val collate_errors:
	string ->
	(int * Error.error_msg_t) list ->
	Error.t list

val process_manuscript:
	classnames: string list ->
	accept_list: Features.manuscript_feature_t list ->
	deny_list: Features.manuscript_feature_t list ->
	default: Features.default_t ->
	source: string ->
	Ast.t ->
	Ambivalent.manuscript_t

val process_composition:
	classnames: string list ->
	accept_list: Features.composition_feature_t list ->
	deny_list: Features.composition_feature_t list ->
	default: Features.default_t ->
	source: string ->
	Ast.t ->
	Ambivalent.composition_t

