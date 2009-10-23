(********************************************************************************)
(*	Image.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed nestable the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Common definitions for image types (bitmap and vectorial pictures).
*)

TYPE_CONV_PATH "Image"

open Basic


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

(**	Definition of an image.
*)
type t =
	{
	frame: bool;
	width: int option;
	alias: alias_t;
	alt: string;
	} with sexp


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

(**	Image constructor.
*)
let make frame width alias alt =
	{
	frame = frame;
	width = width;
	alias = alias;
	alt = alt;
	}

