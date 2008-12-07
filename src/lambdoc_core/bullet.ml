(********************************************************************************)
(*	Implementation file for Bullet module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	The various sorts of bullets accepted for unordered lists.
	Note that these map directly into their CSS counterparts.
*)

TYPE_CONV_PATH "Document"


(********************************************************************************)
(**	{Exceptions}								*)
(********************************************************************************)

exception Unknown_bullet of string


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type t =
	| Default
	| Disc
	| Circle
	| Square
	| None
	(*with sexp*)


(********************************************************************************)
(**	{2 Public values and functions}						*)
(********************************************************************************)

let of_string = function
	| "disc"	-> Disc
	| "circle"	-> Circle
	| "square"	-> Square
	| "none"	-> None
	| other		-> raise (Unknown_bullet other)

let to_string = function
	| Default
	| Disc		-> "disc"
	| Circle	-> "circle"
	| Square	-> "square"
	| None		-> "none"

