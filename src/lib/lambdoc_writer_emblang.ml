open Lambdoc_document.Valid


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type token =
    | Special
    | Normal of string


(********************************************************************************)
(** {1 Private functions and values}                                            *)
(********************************************************************************)

let tokenize lexbuf =
    let aggregate x1 = function
        | (Normal x2) :: tl -> (Normal (x2 ^ x1)) :: tl
        | xs                -> (Normal x1) :: xs in
    let rec tokenize_aux accum = match%sedlex lexbuf with
        | '\\', any                 -> tokenize_aux (aggregate (Sedlexing.Utf8.sub_lexeme lexbuf 1 1) accum)
        | '#'                       -> tokenize_aux (Special :: accum)
        | Plus (Compl ('#' | '\\')) -> tokenize_aux (aggregate (Sedlexing.Utf8.lexeme lexbuf) accum)
        | eof                       -> accum
        | _                         -> assert false
    in List.rev (tokenize_aux [])


let rec process = function
    | Normal s :: tl                       -> Inline.plain s :: process tl
    | Special :: Normal s :: Special :: tl -> Inline.mono [Inline.plain s] :: process tl
    | []                                   -> []
    | _                                    -> assert false


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let convert expl = match Sedlexing.Utf8.from_string expl |> tokenize |> process with
    | []  -> failwith "Emblang.convert"
    | seq -> seq

