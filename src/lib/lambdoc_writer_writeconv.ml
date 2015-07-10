(********************************************************************************)
(*  Lambdoc_writer_writeconv.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Utility functions for converting {!Lambdoc_core} values to strings.
*)

module List = BatList
module String = BatString

open Lambdoc_core


(********************************************************************************)
(** {1 Submodule definitions}                                                   *)
(********************************************************************************)

(********************************************************************************)
(** {2 Source values}                                                           *)
(********************************************************************************)

(********************************************************************************)
(** {2 Tabular values}                                                          *)
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
(** {2 Order values}                                                            *)
(********************************************************************************)

module Order_output =
struct
    type ordinal_converter_t = Order.ordinal_t -> string
    type hierarchical_converter_t = (int -> string) list

    (** This function converts an ordinal number into a sequence of uppercase
        letters used for numbering appendices.  Ordinal 1 is converted to "A",
        26 to "Z", 27 to "AA", and so forth (note that this is not quite the
        same as conversion to base 26).  This function is the inverse of
        {!int_of_alphaseq}.
    *)
    let alphaseq_of_int num =
        let base = 26 in
        let rec from_base10 num =
            let num = num - 1 in
            if num < base
            then
                [num]
            else
                let rem = num mod base
                and num = num / base in
                rem::(from_base10 num) in
        let alpha_of_int num =
            String.of_char (char_of_int (65 + num)) in
        let rems = from_base10 num in
        List.fold_left (^) "" (List.rev_map alpha_of_int rems)

    (** Converts an integer into its roman numeral representation.
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
            | _ -> assert false in
        let rec to_roman i =
            if i <= 0
            then assert false
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

    let format_mainbody = List.make 6 string_of_int

    let format_appendixed = alphaseq_of_int :: List.make 5 string_of_int

    let maybe_string_of_ordinal conv = function
        | `Auto_given o
        | `User_given o -> Some (conv o)
        | `None_given   -> None

    let maybe_string_of_hierarchical conv = function
        | `Auto_given order
        | `User_given order ->
            let rec mapper fs ls = match (fs, ls) with
                | (fhd :: ftl, lhd :: ltl) -> fhd lhd :: mapper ftl ltl
                | _            -> [] in
            Some (String.join "." (mapper conv order))
        | `None_given ->
            None
end


(********************************************************************************)
(** {2 Math values}                                                             *)
(********************************************************************************)

module Math_output =
struct
    open Math

    exception Mathtex_undefined
    exception Mathml_undefined

    let get_mathtex = function
        | Mathtex str   -> str
        | Mathml _      -> raise Mathtex_undefined
        | Both (str, _) -> str

    let get_mathml = function
        | Mathtex _     -> raise Mathml_undefined
        | Mathml str    -> str
        | Both (_, str) -> str
end

