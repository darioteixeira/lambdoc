(********************************************************************************)
(*	Implementation file for Invalid module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(*TYPE_CONV_PATH "Document"*)


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type invalid_t = Error.t list (*with sexp*)

type 'a t = invalid_t (*with sexp*)

type manuscript_t = [`Manuscript] t (*with sexp*)

type composition_t = [`Composition] t (*with sexp*)


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

let make_manuscript errors = errors

let make_composition errors = errors

