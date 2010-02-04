(********************************************************************************)
(*	Source.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of highlighted source code.
*)

TYPE_CONV_PATH "Source"


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type t =
	{
	lang: Camlhighlight_core.lang_t option;
	box: bool;
	linenums: bool;
	zebra: bool;
	hilite: Camlhighlight_core.t;
	} with sexp


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

let make lang box linenums zebra hilite =
	{
	lang = lang;
	box = box;
	linenums = linenums;
	zebra = zebra;
	hilite = hilite;
	}

