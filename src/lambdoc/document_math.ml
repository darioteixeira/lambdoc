(********************************************************************************)
(*	Implementation file for Document_math.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed nestable the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of the module that handles document math.
*)

TYPE_CONV_PATH "Document"

open Document_basic


(********************************************************************************)
(*	{2 Math module}								*)
(********************************************************************************)

module Math:
sig
	exception Invalid_mathtex
	exception Invalid_mathml

	type t with sexp

	val from_mathtex: string -> t
	val from_mathml: string -> t
	val to_inline_xhtml: t -> [> `Span ] XHTML.M.elt
	val to_block_xhtml: t -> [> `Div ] XHTML.M.elt
end =
struct
	exception Invalid_mathtex
	exception Invalid_mathml

	type t =
		{
		mathtex: string option;
		mathml: string;
		} with sexp

	let from_mathtex txt =
		{
		mathtex = Some txt;
		mathml = Blahcaml.safe_mathml_from_tex txt;
		}

	let from_mathml txt =
		{
		mathtex = None;
		mathml = Blahcaml.sanitize_mathml txt;
		}

	let to_inline_xhtml math =
		XHTML.M.unsafe_data math.mathml

	let to_block_xhtml math =
		XHTML.M.unsafe_data math.mathml
end

