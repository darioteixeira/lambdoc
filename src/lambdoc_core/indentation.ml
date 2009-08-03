(********************************************************************************)
(*	Indentation.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of possible indentations for document paragraphs.
*)

TYPE_CONV_PATH "Indentation"


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type t =
	| Indent_true
	| Indent_false
	| Indent_default
	with sexp

