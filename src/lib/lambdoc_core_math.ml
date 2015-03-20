(********************************************************************************)
(*	Lambdoc_core_math.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type mathtex_t = string
type mathml_t = string

type t =
	| Mathtex of mathtex_t
	| Mathml of mathml_t
	| Both of mathtex_t * mathml_t

