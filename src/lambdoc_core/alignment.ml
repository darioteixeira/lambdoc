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
(**	{Exceptions}								*)
(********************************************************************************)

exception Unknown_alignment of string


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type t =
	| Center
	| Left
	| Right (*with sexp*)


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

let of_string = function
	| "center"	-> Center
	| "left"	-> Left
	| "right"	-> Right
	| other		-> raise (Unknown_alignment other)

let to_string = function
	| Center	-> "center"
	| Left		-> "left"
	| Right		-> "right"

