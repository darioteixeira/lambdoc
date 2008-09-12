(********************************************************************************)
(**	Postprocessing on documents.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Document_error
open Document_ambivalent


(********************************************************************************)
(*	{2 Public functions}							*)
(********************************************************************************)

val collate_errors: string -> (int * Error.error_msg_t) list -> Error.t list
val process_manuscript: string -> Document_ast.t -> Ambivalent.manuscript_t
val process_composition: string -> Document_ast.t -> Ambivalent.composition_t

