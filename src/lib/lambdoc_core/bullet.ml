(********************************************************************************)
(*	Bullet.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	The various sorts of bullets accepted for unordered lists.
	Note that these map directly into their CSS counterparts.
*)

TYPE_CONV_PATH "Bullet"


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type t =
	| Disc
	| Circle
	| Square
	| None
	with sexp


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

let of_string = function
	| "disc"	-> Disc
	| "circle"	-> Circle
	| "square"	-> Square
	| "none"	-> None
	| other		-> invalid_arg "Unknown bullet"

let to_string = function
	| Disc		-> "disc"
	| Circle	-> "circle"
	| Square	-> "square"
	| None		-> "none"

