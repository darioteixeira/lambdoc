(********************************************************************************)
(*  Lambdoc_writer_emblang.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type token =
    | Code
    | Plain of string


(********************************************************************************)
(** {1 Private functions and values}                                            *)
(********************************************************************************)

let tokenize lexbuf =
    let aggregate x1 = function
        | (Plain x2) :: tl -> (Plain (x2 ^ x1)) :: tl
        | xs               -> (Plain x1) :: xs in
    let rec tokenize_aux accum = match%sedlex lexbuf with
        | '\\', any                 -> tokenize_aux (aggregate (Sedlexing.Utf8.sub_lexeme lexbuf 1 1) accum)
        | '#'                       -> tokenize_aux (Code :: accum)
        | Plus (Compl ('#' | '\\')) -> tokenize_aux (aggregate (Sedlexing.Utf8.lexeme lexbuf) accum)
        | eof                       -> accum
        | _                         -> assert false
    in List.rev (tokenize_aux [])


let rec process = function
    | Plain s :: tl                 -> Inline.plain s :: process tl
    | Code :: Plain s :: Code :: tl -> Inline.code [Inline.plain s] :: process tl
    | []                            -> []
    | _                             -> assert false


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let convert expl = match Sedlexing.Utf8.from_string expl |> tokenize |> process with
    | []  -> failwith "Emblang.convert"
    | seq -> seq

