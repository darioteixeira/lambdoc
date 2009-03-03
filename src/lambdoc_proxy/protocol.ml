(********************************************************************************)
(*	Interface file for Protocol module.

	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(********************************************************************************)
(*	{2 Type definitions}							*)
(********************************************************************************)

type request_t =
	| Manuscript_from_lambtex of string
	| Composition_from_lambtex of string

