(********************************************************************************)
(*	Design.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	The various sorts of designs accepted for custom environments.
*)

TYPE_CONV_PATH "Design"


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type t =
	| Inside
	| Box
	| Hline
	with sexp


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

let of_string = function
	| "inside" -> Inside
	| "box"	   -> Box
	| "hline"  -> Hline
	| _	-> invalid_arg "Unknown design"

let to_string = function
	| Inside -> "inside"
	| Box	 -> "box"
	| Hline  -> "hline"

