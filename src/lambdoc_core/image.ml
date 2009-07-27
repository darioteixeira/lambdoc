(********************************************************************************)
(*	Image.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed nestable the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

TYPE_CONV_PATH "Image"


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

(**	Common definitions for image types (bitmap and vectorial pictures).
*)
type t = bool * int option * alias_t * string with sexp, bin_io

