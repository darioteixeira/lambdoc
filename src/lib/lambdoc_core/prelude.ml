(********************************************************************************)
(*	Prelude.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of common types and functions which though not defined
	in Pervasives, probably should.
*)

TYPE_CONV_PATH "Prelude"


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

(**	The type of non-empty lists.
*)
type 'a plus_t = 'a * 'a list with sexp


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

(**	Similar to [List.map], but for values of type {!plus_t}.
*)
let plusmap f hd tl =
	let new_hd = f hd in
	let new_tl = List.map f tl
	in (new_hd, new_tl)


(**	Possibly apply a function.
*)
let maybe f = function
	| Some x	-> Some (f x)
	| None		-> None

