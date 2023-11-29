open Lambdoc_writer


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type t = Translations.t


(********************************************************************************)
(** {1 Private functions and values}                                            *)
(********************************************************************************)

let parser x = match String.lowercase x with
    | "en" -> `Ok Translations.english_names
    | "fr" -> `Ok Translations.french_names
    | "pt" -> `Ok Translations.portuguese_names
    | x    -> `Error (Printf.sprintf "Unknown language '%s'" x)

let printer fmt x = Format.pp_print_string fmt Translations.(x.description)


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let converter = (parser, printer)

let default = Translations.english_names

