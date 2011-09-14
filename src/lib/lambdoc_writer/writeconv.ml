(********************************************************************************)
(*	Writeconv.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Utility functions for converting {!Lambdoc_core} values to strings.
*)

open ExtString
open Lambdoc_core


(********************************************************************************)
(**	{1 Submodule definitions}						*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Basic values}							*)
(********************************************************************************)

module Basic_output =
struct
	open Basic

	let string_of_bullet = function
		| Bullet.Disc	-> "disc"
		| Bullet.Circle -> "circle"
		| Bullet.Square -> "square"
		| Bullet.None	-> "none"


	let string_of_numbering = function
		| Numbering.Decimal	-> "decimal"
		| Numbering.Lower_roman -> "lower-roman"
		| Numbering.Upper_roman -> "upper-roman"
		| Numbering.Lower_alpha -> "lower-alpha"
		| Numbering.Upper_alpha -> "upper-alpha"
		| Numbering.None	-> "none"


	let string_of_floatation = function
		| Floatation.Center -> "center"
		| Floatation.Left   -> "left"
		| Floatation.Right  -> "right"
end


(********************************************************************************)
(**	{2 Book values}								*)
(********************************************************************************)

module Book_output =
struct
	external string_of_isbn: Book.isbn_t -> string = "%identity"
end


(********************************************************************************)
(**	{2 Source values}							*)
(********************************************************************************)

module Source_output =
struct
	open Source

	let string_of_style = function
		| Plain   -> "plain"
		| Boxed   -> "boxed"
		| Zebra   -> "zebra"
		| Console -> "console"
end


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
	open Order

	type ordinal_converter_t = (ordinal_t -> string)

	type hierarchical_converter_t =
		{
		level1: (int -> string);
		level2: (int -> string);
		level3: (int -> string);
		}


	(**	This function converts an ordinal number into a sequence of uppercase
		letters used for numbering appendices.  Ordinal 1 is converted to "A",
		26 to "Z", 27 to "AA", and so forth (note that this is not quite the
		same as conversion to base 26).  This function is the inverse of
		{!int_of_alphaseq}.
	*)
	let alphaseq_of_int num =
		let base = 26 in
		let rec from_base10 num =
			let num = num - 1
			in if num < base
			then
				[num]
			else
				let rem = num mod base
				and num = num / base
				in rem::(from_base10 num) in
		let alpha_of_int num =
			String.of_char (char_of_int (65 + num)) in
		let rems = from_base10 num
		in List.fold_left (^) "" (List.rev_map alpha_of_int rems)


	(**	Converts an integer into its roman numeral representation.
	*)
	let roman_of_int i =
		let digit x y z = function
			| 1 -> [x]
			| 2 -> [x; x]
			| 3 -> [x; x; x]
			| 4 -> [x; y]
			| 5 -> [y]
			| 6 -> [y; x]
			| 7 -> [y; x; x]
			| 8 -> [y; x; x; x]
			| 9 -> [x; z]
			| _ -> invalid_arg "Invalid digit" in
		let rec to_roman i =
			if i <= 0
			then invalid_arg "Invalid roman numeral"
			else if i >= 1000
				then 'M' :: to_roman (i - 1000)
				else if i >= 100
					then digit 'C' 'D' 'M' (i / 100) @ to_roman (i mod 100)
					else if i >= 10
						then digit 'X' 'L' 'C' (i / 10) @ to_roman (i mod 10)
						else digit 'I' 'V' 'X' i
		in String.implode (to_roman i)


	let format_arabic = string_of_int


	let format_roman = roman_of_int


	let format_mainbody =
		{
		level1 = string_of_int;
		level2 = string_of_int;
		level3 = string_of_int;
		}


	let format_appendixed =
		{
		level1 = alphaseq_of_int;
		level2 = string_of_int;
		level3 = string_of_int;
		}


	let maybe_string_of_ordinal conv = function
		| `Auto_given o
		| `User_given o	-> Some (conv o)
		| `None_given	-> None


	let maybe_string_of_hierarchical conv = function
		| `Auto_given (Level1_order l1)
		| `User_given (Level1_order l1)		  -> Some (conv.level1 l1)
		| `Auto_given (Level2_order (l1, l2))
		| `User_given (Level2_order (l1, l2))	  -> Some ((conv.level1 l1) ^ "." ^ (conv.level2 l2))
		| `Auto_given (Level3_order (l1, l2, l3))
		| `User_given (Level3_order (l1, l2, l3)) -> Some ((conv.level1 l1) ^ "." ^ (conv.level2 l2) ^ "." ^ (conv.level3 l3))
		| `None_given				  -> None
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

