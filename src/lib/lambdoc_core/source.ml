(********************************************************************************)
(*	Source.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of highlighted source code.
*)

open Sexplib.Std


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type t =
	{
	lang: Camlhighlight_core.lang_t option;
	hilite: Camlhighlight_core.t;
	linenums: bool;
	} with sexp


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

let make lang hilite linenums =
	{lang; hilite; linenums}

