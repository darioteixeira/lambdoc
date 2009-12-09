(********************************************************************************)
(*	Numbering.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	The various sorts of numbering accepted for ordered lists.
	Note that these map directly into their CSS counterparts.
*)

TYPE_CONV_PATH "Numbering"


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type t =
	| Decimal
	| Lower_roman
	| Upper_roman
	| Lower_alpha
	| Upper_alpha
	| None
	with sexp


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

let of_string = function
	| "decimal" | "0"	-> Decimal
	| "lower-roman" | "i"	-> Lower_roman
	| "upper-roman" | "I"	-> Upper_roman
	| "lower-alpha" | "a"	-> Lower_alpha
	| "upper-alpha" | "A"	-> Upper_alpha
	| "none"		-> None
	| other			-> invalid_arg "Unknown numbering"

let to_string = function
	| Decimal	-> "decimal"
	| Lower_roman	-> "lower-roman"
	| Upper_roman	-> "upper-roman"
	| Lower_alpha	-> "lower-alpha"
	| Upper_alpha	-> "upper-alpha"
	| None		-> "none"

