(********************************************************************************)
(*	Markup.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type class_t = [ `Manuscript | `Composition ]
type input_t = [ `Lambtex | `Lamblite | `Lambhtml | `Sexp ]
type output_t = [ `Sexp | `Xhtml ]


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

let input_of_string x = match String.lowercase x with
	| "lambtex"  -> `Lambtex
	| "lamblite" -> `Lamblite
	| "lambhtml" -> `Lambhtml
	| "sexp"     -> `Sexp
	| _	     -> invalid_arg x


let output_of_string x = match String.lowercase x with
	| "sexp"  -> `Sexp
	| "xhtml" -> `Xhtml
	| _	  -> invalid_arg x


let to_string = function
	| `Lambtex  -> "Lambtex"
	| `Lamblite -> "Lamblite"
	| `Lambhtml -> "Lambhtml"
	| `Sexp     -> "Sexp"
	| `Xhtml    -> "Xhtml"

