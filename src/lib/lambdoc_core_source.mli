(********************************************************************************)
(*	Lambdoc_core_source.mli
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of highlighted source code.
*)


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type t =
	{
	lang: string option;
	hilite: string;
	linenums: bool;
	}


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

val make: string option -> string -> bool -> t

