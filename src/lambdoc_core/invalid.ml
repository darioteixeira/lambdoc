(********************************************************************************)
(*	Implementation file for Invalid module.

	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

TYPE_CONV_PATH "Invalid"


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type manuscript_t = Error.t list with sexp, bin_io
type composition_t = Error.t list with sexp, bin_io


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

let make_manuscript errors = errors
let make_composition errors = errors

