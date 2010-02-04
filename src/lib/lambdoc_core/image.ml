(********************************************************************************)
(*	Image.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Common definitions for image types (bitmap and vectorial pictures).
*)

TYPE_CONV_PATH "Image"

open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type t =
	{
	frame: bool;
	width: int option;
	alias: Alias.t;
	alt: string;
	} with sexp


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

let make frame width alias alt =
	{
	frame = frame;
	width = width;
	alias = alias;
	alt = alt;
	}

