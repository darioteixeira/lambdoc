(********************************************************************************)
(*	Program.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed nestable the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of highlighted source code.
*)

TYPE_CONV_PATH "Program"


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

(**	Complete definition of highlighted source code samples.
*)
type t =
	{
	lang: Camlhighlight_core.lang_t option;
	linenums: bool;
	zebra: bool;
	hilite: Camlhighlight_core.t;
	} with sexp


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

(**	Code constructor.
*)
let make lang linenums zebra hilite =
	{
	lang = lang;
	linenums = linenums;
	zebra = zebra;
	hilite = hilite;
	}

