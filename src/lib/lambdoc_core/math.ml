(********************************************************************************)
(*	Math.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of the module that encodes document math.
*)

TYPE_CONV_PATH "Math"


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type mathtex_t = string with sexp
type mathml_t = string with sexp

type t =
	| Mathtex of mathtex_t
	| Mathml of mathml_t
	| Both of mathtex_t * mathml_t
	with sexp

