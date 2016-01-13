(********************************************************************************)
(*  Language.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_writer


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type t = Translations.t


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

let of_string x = match String.lowercase x with
    | "en" -> Translations.english_names
    | "fr" -> Translations.french_names
    | "pt" -> Translations.portuguese_names
    | x    -> invalid_arg ("Language.of_string: " ^ x)


let default = Translations.english_names

