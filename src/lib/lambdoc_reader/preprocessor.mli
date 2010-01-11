(********************************************************************************)
(*	Preprocessor.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Preprocessing on a document source.
*)

(********************************************************************************)
(**	{1 Exceptions}								*)
(********************************************************************************)

exception Malformed_source of string * int list


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

val verify_utf8: string -> unit

