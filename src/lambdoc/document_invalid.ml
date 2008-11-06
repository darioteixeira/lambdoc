(********************************************************************************)
(*	Implementation file for Document_invalid.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of invalid documents.
*)

TYPE_CONV_PATH "Document"

open Document_error


(********************************************************************************)
(**	{2 Invalid module}							*)
(********************************************************************************)

module Invalid:
sig
	type invalid_t = Error.t list with sexp

	type 'a t = invalid_t with sexp

	type manuscript_t = [`Manuscript] t with sexp

	type composition_t = [`Composition] t with sexp

	val make_manuscript: Error.t list -> manuscript_t

	val make_composition: Error.t list -> composition_t
end =
struct
	type invalid_t = Error.t list with sexp

	type 'a t = invalid_t with sexp

	type manuscript_t = [`Manuscript] t with sexp

	type composition_t = [`Composition] t with sexp

	let make_manuscript errors = errors

	let make_composition errors = errors
end

