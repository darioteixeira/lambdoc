(********************************************************************************)
(*	Writeconv.mli
	Copyright (c) 2009-2013 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Utility functions for converting {!Lambdoc_core} values to strings.
*)

open Lambdoc_core


(********************************************************************************)
(**	{1 Submodule definitions}						*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Basic values}							*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Book values}								*)
(********************************************************************************)

module Book_output:
sig
	open Book

	val string_of_isbn: isbn_t -> string
end


(********************************************************************************)
(**	{2 Tabular values}							*)
(********************************************************************************)

module Tabular_output:
sig
	val string_of_alignment: Tabular.alignment_t -> string
end


(********************************************************************************)
(**	{2 Order values}							*)
(********************************************************************************)

module Order_output:
sig
	open Order

	type ordinal_converter_t = (ordinal_t -> string)

	type hierarchical_converter_t =
		{
		level1: (int -> string);
		level2: (int -> string);
		level3: (int -> string);
		}

	val format_arabic: ordinal_converter_t
	val format_roman: ordinal_converter_t
	val format_mainbody: hierarchical_converter_t
	val format_appendixed: hierarchical_converter_t

	val maybe_string_of_ordinal: ordinal_converter_t -> (ordinal_t, 'b) t -> string option
	val maybe_string_of_hierarchical: hierarchical_converter_t -> (hierarchical_t, 'b) t -> string option
end


(********************************************************************************)
(**	{2 Math values}								*)
(********************************************************************************)

module Math_output:
sig
	open Math

	exception Mathtex_undefined
	exception Mathml_undefined

	val get_mathtex: t -> mathtex_t
	val get_mathml: t -> mathml_t
end

