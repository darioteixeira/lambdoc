(********************************************************************************)
(*	Math.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed nestable the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

TYPE_CONV_PATH "Math"


(********************************************************************************)
(**	{2 Exceptions}								*)
(********************************************************************************)

exception Mathtex_undefined
exception Mathml_undefined


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type mathtex_t = string with sexp, bin_io
type mathml_t = string with sexp, bin_io

type t =
	| Mathtex of mathtex_t
	| Mathml of mathml_t
	| Both of mathtex_t * mathml_t
	with sexp, bin_io


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

let from_mathtex mathtex =
	Mathtex mathtex

let from_mathml mathml =
	Mathml mathml

let from_both mathtex mathml =
	Both (mathtex, mathml)

let get_mathtex = function
	| Mathtex str	-> str
	| Mathml _	-> raise Mathtex_undefined
	| Both (str, _)	-> str

let get_mathml = function
	| Mathtex _	-> raise Mathml_undefined
	| Mathml str	-> str
	| Both (_, str)	-> str

