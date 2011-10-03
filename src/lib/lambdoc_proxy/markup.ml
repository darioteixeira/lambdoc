(********************************************************************************)
(*	Markup.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type t =
	| Lambtex
	| Lambhtml
	| Lamblite


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

let of_string = function
	| "lambtex"  -> Lambtex
	| "lambhtml" -> Lambhtml
	| "lamblite" -> Lamblite
	| x	     -> invalid_arg ("Markup.of_string: " ^ x)


let to_string = function
	| Lambtex  -> "lambtex"
	| Lambhtml -> "lambhtml"
	| Lamblite -> "lamblite"

