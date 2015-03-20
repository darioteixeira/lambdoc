(********************************************************************************)
(*	Lambdoc_writer_writeconv.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
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
(**	{2 Source values}							*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Tabular values}							*)
(********************************************************************************)

module Tabular_output =
struct
	open Tabular

	let string_of_alignment = function
		| Center  -> "c"
		| Left    -> "l"
		| Right   -> "r"
		| Justify -> "j"
end


(********************************************************************************)
(**	{2 Order values}							*)
(********************************************************************************)

module Order_output =
struct
	type ordinal_converter_t = Order.ordinal_t -> string
	type hierarchical_converter_t = (int -> string) list

	(**	This function converts an ordinal number into a sequence of uppercase
		letters used for numbering appendices.  Ordinal 1 is converted to "A",
		26 to "Z", 27 to "AA", and so forth (note that this is not quite the
		same as conversion to base 26).  This function is the inverse of
		{!int_of_alphaseq}.
	*)
	let alphaseq_of_int num = "x"

	(**	Converts an integer into its roman numeral representation.
	*)
	let roman_of_int i = "x"

	let format_arabic = string_of_int

	let format_roman = roman_of_int

	let format_mainbody = []

	let format_appendixed = [alphaseq_of_int]

	let maybe_string_of_ordinal conv = function
		| `Auto_given o
		| `User_given o	-> Some (conv o)
		| `None_given	-> None

	let maybe_string_of_hierarchical conv = function
		| `Auto_given order
		| `User_given order ->
			let rec mapper fs ls = match (fs, ls) with
				| (fhd :: ftl, lhd :: ltl) -> fhd lhd :: mapper ftl ltl
				| _			   -> [] in
			Some (String.concat "." (mapper conv order))
		| `None_given ->
			None
end


(********************************************************************************)
(**	{2 Math values}								*)
(********************************************************************************)

module Math_output =
struct
	open Math

	exception Mathtex_undefined
	exception Mathml_undefined

	let get_mathtex = function
		| Mathtex str	-> str
		| Mathml _	-> raise Mathtex_undefined
		| Both (str, _)	-> str

	let get_mathml = function
		| Mathtex _	-> raise Mathml_undefined
		| Mathml str	-> str
		| Both (_, str)	-> str
end

