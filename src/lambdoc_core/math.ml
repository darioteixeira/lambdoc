(********************************************************************************)
(*	Implementation file for Math module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed nestable the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(*TYPE_CONV_PATH "Document"*)


(********************************************************************************)
(**	{Exceptions}								*)
(********************************************************************************)

exception Invalid_mathtex
exception Invalid_mathml


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type t =
	{
	mathtex: string option;
	mathml: string;
	} (*with sexp*)


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

let from_mathtex txt =
	try
		{
		mathtex = Some txt;
		mathml = Blahcaml.safe_mathml_from_tex txt;
		}
	with
		| Blahcaml.Blahtex_error _	-> raise Invalid_mathtex
		| Blahcaml.Unicode_error 	-> raise Invalid_mathtex
		| _				-> raise Invalid_mathtex

let from_mathml txt =
	try
		{
		mathtex = None;
		mathml = Blahcaml.sanitize_mathml txt;
		}
	with
		| _				-> raise Invalid_mathml

let to_inline_xhtml math =
	XHTML.M.unsafe_data math.mathml

let to_block_xhtml math =
	XHTML.M.unsafe_data math.mathml

