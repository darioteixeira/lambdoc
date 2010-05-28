(********************************************************************************)
(*	Source.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
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

type style_t =
	| Plain		(** No decorations at all; good for run-in source blocks. *)
	| Boxed		(** Source should be framed inside a box. *)
	| Zebra		(** Use Zebra pattern; implies framing inside box. *)
	| Console	(** Source actually represents console "screenshot". *)
	with sexp

type t =
	{
	lang: Camlhighlight_core.lang_t option;
	hilite: Camlhighlight_core.t;
	style: style_t;
	linenums: bool;
	} with sexp


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

let make lang hilite style linenums =
	{
	lang = lang;
	hilite = hilite;
	style = style;
	linenums = linenums;
	}

