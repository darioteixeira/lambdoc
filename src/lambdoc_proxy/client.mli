(********************************************************************************)
(*	Interface file for Client module.

	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(********************************************************************************)
(*	{2 Public functions and values}						*)
(********************************************************************************)

val manuscript_from_lambtex: string -> Lambdoc_core.Ambivalent.manuscript_t Lwt.t
val composition_from_lambtex: string -> Lambdoc_core.Ambivalent.composition_t Lwt.t

