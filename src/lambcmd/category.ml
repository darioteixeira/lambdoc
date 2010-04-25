(********************************************************************************)
(*	Category.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type t = [ `Manuscript | `Composition ]


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

let of_string x = match String.lowercase x with
	| "manuscript" -> `Manuscript
	| "composition" -> `Composition
	| _		-> invalid_arg "Category.of_string"


let default = `Manuscript
