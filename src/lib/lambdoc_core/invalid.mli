(********************************************************************************)
(*	Invalid.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning invalid documents.
*)

open Prelude


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type manuscript_t = Error.t nelist with sexp
type composition_t = Error.t nelist with sexp


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

val make_manuscript: Error.t nelist -> manuscript_t
val make_composition: Error.t nelist -> composition_t

