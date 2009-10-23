(********************************************************************************)
(*	Math.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed nestable the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of the module that encodes document math.
*)


(********************************************************************************)
(**	{Exceptions}								*)
(********************************************************************************)

exception Mathtex_undefined
exception Mathml_undefined


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type mathtex_t = string with sexp
type mathml_t = string with sexp

type t with sexp


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

val from_mathtex: mathtex_t -> t
val from_mathml: mathml_t -> t
val from_both: mathtex_t -> mathml_t -> t

val get_mathtex: t -> mathtex_t
val get_mathml: t -> mathml_t

