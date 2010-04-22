(********************************************************************************)
(*	Language.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_writer


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type t = Translations.t


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

let of_string x = match String.lowercase x with
	| "en" -> Translations.english_names
	| "fr" -> Translations.french_names
	| "pt" -> Translations.portuguese_names
	| _    -> invalid_arg "Language.of_string"


let default = Translations.english_names

