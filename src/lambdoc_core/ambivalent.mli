(********************************************************************************)
(*	Interface file for Ambivalent module.

	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning ambivalent documents.
*)

(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type 'a t =
	[ `Valid of 'a Valid.t
	| `Invalid of 'a Invalid.t
	] (*with sexp*)


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

type manuscript_t = [ `Manuscript ] t (*with sexp*)
type composition_t = [ `Composition ] t (*with sexp*)

