(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type input = [ `Lambtex | `Lambwiki | `Lambxml | `Markdown | `Sexp ]
type output = [ `Sexp | `Html ]


(********************************************************************************)
(** {1 Private functions and values}                                            *)
(********************************************************************************)

let input_parser x = match String.lowercase x with
    | "lambtex" | "tex"   -> `Ok `Lambtex
    | "lambwiki" | "wiki" -> `Ok `Lambwiki
    | "lambxml" | "xml"   -> `Ok `Lambxml
    | "markdown" | "md"   -> `Ok `Markdown
    | "sexp"              -> `Ok `Sexp
    | _                   -> `Error (Printf.sprintf "Unknown input markup '%s'" x)

let output_parser x = match String.lowercase x with
    | "sexp" -> `Ok `Sexp
    | "html" -> `Ok `Html
    | x      -> `Error (Printf.sprintf "Unknown output markup '%s'" x)

let to_string = function
    | `Lambtex  -> "Lambtex"
    | `Lambwiki -> "Lambwiki"
    | `Lambxml  -> "Lambxml"
    | `Markdown -> "Markdown"
    | `Sexp     -> "Sexp"
    | `Html     -> "Html"

let printer fmt x =
    Format.pp_print_string fmt (to_string x)


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let input_converter = (input_parser, printer)
let output_converter = (output_parser, printer)

