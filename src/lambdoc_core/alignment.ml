(********************************************************************************)
(*	Implementation file for Alignment module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of possible alignments for document floaters.
*)

TYPE_CONV_PATH "Document"


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type t =
	| Center
	| Left
	| Right with sexp


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

let of_string = function
	| "center"	-> Center
	| "left"	-> Left
	| "right"	-> Right
	| other		-> invalid_arg "Unknown alignment"

let to_string = function
	| Center	-> "center"
	| Left		-> "left"
	| Right		-> "right"

