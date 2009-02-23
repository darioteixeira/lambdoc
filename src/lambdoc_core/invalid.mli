(********************************************************************************)
(*	Interface file for Invalid module.

	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning invalid documents.
*)


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type manuscript_t = Error.t list with sexp
type composition_t = Error.t list with sexp


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

val make_manuscript: Error.t list -> manuscript_t
val make_composition: Error.t list -> composition_t

