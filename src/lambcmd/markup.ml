(********************************************************************************)
(*  Markup.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type input = [ `Lambtex | `Lamblite | `Lambxml | `Markdown | `Sexp ]

type output = [ `Sexp | `Html ]


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

let input_of_string x = match String.lowercase x with
    | "lambtex" | "tex"   -> `Lambtex
    | "lamblite" | "lite" -> `Lamblite
    | "lambxml" | "xml"   -> `Lambxml
    | "markdown" | "md"   -> `Markdown
    | "sexp"              -> `Sexp
    | _                   -> invalid_arg ("Markup.input_of_string: " ^ x)

let output_of_string x = match String.lowercase x with
    | "sexp" -> `Sexp
    | "html" -> `Html
    | x      -> invalid_arg ("Markup.output_of_string: " ^ x)

let to_string = function
    | `Lambtex  -> "Lambtex"
    | `Lamblite -> "Lamblite"
    | `Lambxml  -> "Lambxml"
    | `Markdown -> "Markdown"
    | `Sexp     -> "Sexp"
    | `Html     -> "Html"

