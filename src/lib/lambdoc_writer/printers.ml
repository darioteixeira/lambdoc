(********************************************************************************)
(*	Printers.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open ExtString
open Lambdoc_core


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Conversion functions}						*)
(********************************************************************************)

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


(********************************************************************************)
(**	{2 Printers}								*)
(********************************************************************************)

let arabic = string_of_int

let roman = roman_of_int

let mainbody =
	{
	Order.level1 = string_of_int;
	Order.level2 = string_of_int;
	Order.level3 = string_of_int;
	}

let appendixed =
	{
	Order.level1 = alphaseq_of_int;
	Order.level2 = string_of_int;
	Order.level3 = string_of_int;
	}

