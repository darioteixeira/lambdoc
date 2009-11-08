(********************************************************************************)
(*	Floatation.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of possible floatation options for document blocks.
*)

TYPE_CONV_PATH "Floatation"


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type t =
	| Center
	| Left
	| Right
	with sexp


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

let of_string = function
	| "center"	-> Center
	| "left"	-> Left
	| "right"	-> Right
	| other		-> invalid_arg "Floatation.of_string"

let to_string = function
	| Center	-> "center"
	| Left		-> "left"
	| Right		-> "right"

