(********************************************************************************)
(*	Lambdoc_writer_writeconv.mli
	Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
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
	type ordinal_converter_t = Order.ordinal_t -> string
	type hierarchical_converter_t = (int -> string) list

	val format_arabic: ordinal_converter_t
	val format_roman: ordinal_converter_t
	val format_mainbody: hierarchical_converter_t
	val format_appendixed: hierarchical_converter_t

	val maybe_string_of_ordinal: ordinal_converter_t -> (Order.ordinal_t, 'b) Order.t -> string option
	val maybe_string_of_hierarchical: hierarchical_converter_t -> (Order.hierarchical_t, 'b) Order.t -> string option
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

