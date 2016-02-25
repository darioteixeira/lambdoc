(********************************************************************************)
(*  Lambdoc_rlambwiki_lexer.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_prelude


(********************************************************************************)
(*  {1 Type definitions}                                                        *)
(********************************************************************************)

type listing =
    | Ulist
    | Olist

type text =
    | Plain of string
    | Entity of string
    | Bold_mark
    | Emph_mark
    | Sup_mark
    | Sub_mark
    | Begin_caps | End_caps
    | Begin_mono | End_mono
    | Begin_link | End_link | Link_sep

type line =
    | Begin_source of string | End_source
    | Begin_verbatim of string | End_verbatim
    | Raw of string
    | Section of int * text list
    | Par of int * (listing * int) option * text list


(********************************************************************************)
(*  {1 Regular expressions}                                                     *)
(********************************************************************************)

let alpha = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']
let deci = [%sedlex.regexp? '0' .. '9']
let blank = [%sedlex.regexp? Chars " \t"]
let non_blank = [%sedlex.regexp? Compl blank]
let section_pat = [%sedlex.regexp? Plus '=']
let quote_pat = [%sedlex.regexp? '>', Star ('>' | blank)]
let list_pat = [%sedlex.regexp? Plus '-' | Plus '#']

let escape = [%sedlex.regexp? '\\']
let bold_mark = [%sedlex.regexp? "**"]
let emph_mark = [%sedlex.regexp? "//"]
let sup_mark = [%sedlex.regexp? "^^"]
let sub_mark = [%sedlex.regexp? "__"]
let begin_caps = [%sedlex.regexp? "(("]
let end_caps = [%sedlex.regexp? "))"]
let begin_mono = [%sedlex.regexp? "{{"]
let end_mono = [%sedlex.regexp? "}}"]
let begin_link = [%sedlex.regexp? "[["]
let end_link = [%sedlex.regexp? "]]"]
let link_sep = [%sedlex.regexp? '|']

let entity = [%sedlex.regexp? '&', Opt '#', Plus (alpha | deci), ';']
let ndash = [%sedlex.regexp? "--"]
let mdash = [%sedlex.regexp? "---"]
let ldquo = [%sedlex.regexp? "``"]
let rdquo = [%sedlex.regexp? "''"]

let begin_source = [%sedlex.regexp? "{{{"]
let end_source = [%sedlex.regexp? "}}}"]
let begin_verbatim = [%sedlex.regexp? "((("]
let end_verbatim = [%sedlex.regexp? ")))"]


(********************************************************************************)
(*  {1 Private functions and values}                                            *)
(********************************************************************************)

let count_char str what =
    String.fold_left (fun accum c -> if c = what then accum+1 else accum) 0 str


let rtrim_lexbuf ~first lexbuf =
    Sedlexing.Utf8.sub_lexeme lexbuf first ((Sedlexing.lexeme_length lexbuf) - first - 1)


let text lexbuf =
    let coalesce accum el = match accum with
        | (Plain x) :: tl -> (Plain (x ^ el)) :: tl
        | _               -> (Plain el) :: accum in
    let rec main_scanner accum = match%sedlex lexbuf with
        | eof         -> accum
        | escape, any -> main_scanner (coalesce accum (Sedlexing.Utf8.sub_lexeme lexbuf 1 1))
        | bold_mark   -> main_scanner (Bold_mark :: accum)
        | emph_mark   -> main_scanner (Emph_mark :: accum)
        | sup_mark    -> main_scanner (Sup_mark :: accum)
        | sub_mark    -> main_scanner (Sub_mark :: accum)
        | begin_caps  -> main_scanner (Begin_caps :: accum)
        | end_caps    -> main_scanner (End_caps :: accum)
        | begin_mono  -> main_scanner (Begin_mono :: accum)
        | end_mono    -> main_scanner (End_mono :: accum)
        | begin_link  -> link_scanner (Begin_link :: accum)
        | end_link    -> main_scanner (End_link :: accum)
        | link_sep    -> main_scanner (Link_sep :: accum)
        | entity      -> main_scanner (Entity (rtrim_lexbuf ~first:1 lexbuf) :: accum)
        | ndash       -> main_scanner (Entity "ndash" :: accum)
        | mdash       -> main_scanner (Entity "mdash" :: accum)
        | ldquo       -> main_scanner (Entity "ldquo" :: accum)
        | rdquo       -> main_scanner (Entity "rdquo" :: accum)
        | any         -> main_scanner (coalesce accum (Sedlexing.Utf8.lexeme lexbuf))
        | _           -> assert false
    and link_scanner accum = match%sedlex lexbuf with
        | eof         -> accum
        | escape, any -> link_scanner (coalesce accum (Sedlexing.Utf8.sub_lexeme lexbuf 1 1))
        | end_link    -> main_scanner (End_link :: accum)
        | link_sep    -> main_scanner (Link_sep :: accum)
        | any         -> link_scanner (coalesce accum (Sedlexing.Utf8.lexeme lexbuf))
        | _           -> assert false
    in List.rev (main_scanner [])


(********************************************************************************)
(*  {1 Public functions and values}                                             *)
(********************************************************************************)

let source lexbuf = match%sedlex lexbuf with
    | end_source, Star blank, eof -> End_source
    | Star any                    -> Raw (Sedlexing.Utf8.lexeme lexbuf)
    | _                           -> assert false


let verbatim lexbuf = match%sedlex lexbuf with
    | end_verbatim, Star blank, eof -> End_verbatim
    | Star any                      -> Raw (Sedlexing.Utf8.lexeme lexbuf)
    | _                             -> assert false


let general lexbuf = match%sedlex lexbuf with
    | begin_source, Star non_blank, Star blank, eof ->
        Begin_source (Sedlexing.Utf8.sub_lexeme lexbuf 3 ((Sedlexing.lexeme_length lexbuf) - 3))
    | end_source, Star blank, eof ->
        End_source
    | begin_verbatim, Star non_blank, Star blank, eof ->
        Begin_verbatim (Sedlexing.Utf8.sub_lexeme lexbuf 3 ((Sedlexing.lexeme_length lexbuf) - 3))
    | end_verbatim, Star blank, eof ->
        End_verbatim
    | section_pat, Opt blank ->
        let lexeme = Sedlexing.Utf8.lexeme lexbuf in
        let section_level = count_char lexeme '=' in
        Section (section_level, text lexbuf)
    | Opt quote_pat, Star blank, Opt (list_pat, Plus blank) ->
        let lexeme = Sedlexing.Utf8.lexeme lexbuf in
        let quote_level = count_char lexeme '>' in
        let list_level =
            let counter = count_char lexeme in
            match (counter '-', counter '#') with
                | (x, 0) when x > 0 -> Some (Ulist, x)
                | (0, x) when x > 0 -> Some (Olist, x)
                | _                 -> None in
        let text = text lexbuf in
        Par (quote_level, list_level, text)
    | _ ->
        assert false

