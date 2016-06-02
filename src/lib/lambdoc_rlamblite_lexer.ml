(********************************************************************************)
(*  Lambdoc_rlamblite_lexer.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_prelude


(********************************************************************************)
(*  {1 Exceptions}                                                              *)
(********************************************************************************)

exception Unterminated_literal of string
exception Bad_literal_prefix of string * string
exception Misaligned_quotation


(********************************************************************************)
(*  {1 Type definitions}                                                        *)
(********************************************************************************)

type inline =
    | Plain of string
    | Entity of string
    | Cite of string list
    | See of string list
    | Bold_mark
    | Emph_mark
    | Sup_mark
    | Sub_mark
    | Ins_mark
    | Del_mark
    | Begin_mono | End_mono
    | Begin_code | End_code
    | Begin_link | End_link | Link_sep

type rule = Single | Double | Star

type literal = Source | Verbatim

type ghost = Sbib | Note

type tprefix =
    | Sec of string
    | Oli of string
    | Uli

type regular =
    | Rule of rule
    | Literal of literal * int * string * string
    | Ghost of ghost * string * inline list
    | Section of string * inline list
    | Textual of tprefix option * inline list

type lexeme =
    | Eof
    | Regular of string * string * regular

type syntax = [ `Lambwiki | `Markdown ]


(********************************************************************************)
(*  {1 Regular expressions}                                                     *)
(********************************************************************************)

let eol = [%sedlex.regexp? '\n']
let alpha = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']
let digit = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? Plus digit]
let numbers = [%sedlex.regexp? Star (number, ','), number]
let blank = [%sedlex.regexp? Chars " \t"]
let style = [%sedlex.regexp? Plus (Compl (blank | eol))]
let escape = [%sedlex.regexp? '\\']


(********************************************************************************)
(*  {1 Private functions and values}                                            *)
(********************************************************************************)

let ltrim_lexbuf ~first lexbuf =
        Sedlexing.Utf8.sub_lexeme lexbuf first ((Sedlexing.lexeme_length lexbuf) - first - 1)

let get_labeldef lexbuf =
    Sedlexing.Utf8.lexeme lexbuf |>
    String.rstrip |>
    String.chop ~left:1 ~right:1

let get_labelref lexbuf =
    String.nsplit_by_char (get_labeldef lexbuf) ','

let scan_text ~syntax lexbuf =
    let add_plain accum el = match accum with
        | (`Plain buf) :: _ ->
            Buffer.add_string buf el;
            accum
        | _ ->
            let buf = Buffer.create 64 in
            Buffer.add_string buf el;
            `Plain buf :: accum in
    let add_other accum el =
        `Other el :: accum in
    let rec lambwiki_main_loop accum = match%sedlex lexbuf with
        | eol | eof                               -> accum
        | escape, eol                             -> accum
        | escape, any                             -> lambwiki_main_loop (add_plain accum (Sedlexing.Utf8.sub_lexeme lexbuf 1 1))
        | "---"                                   -> lambwiki_main_loop (add_other accum (Entity "mdash"))
        | "--"                                    -> lambwiki_main_loop (add_other accum (Entity "ndash"))
        | "``"                                    -> lambwiki_main_loop (add_other accum (Entity "ldquo"))
        | "''"                                    -> lambwiki_main_loop (add_other accum (Entity "rdquo"))
        | "**"                                    -> lambwiki_main_loop (add_other accum Bold_mark)
        | "//"                                    -> lambwiki_main_loop (add_other accum Emph_mark)
        | "^^"                                    -> lambwiki_main_loop (add_other accum Sup_mark)
        | "__"                                    -> lambwiki_main_loop (add_other accum Sub_mark)
        | "++"                                    -> lambwiki_main_loop (add_other accum Ins_mark)
        | "~~"                                    -> lambwiki_main_loop (add_other accum Del_mark)
        | "(("                                    -> lambwiki_main_loop (add_other accum Begin_mono)
        | "))"                                    -> lambwiki_main_loop (add_other accum End_mono)
        | "{{"                                    -> lambwiki_raw_loop (add_other accum Begin_code)
        | "}}"                                    -> lambwiki_main_loop (add_other accum End_code)
        | "[["                                    -> link_loop lambwiki_main_loop (add_other accum Begin_link)
        | "]]"                                    -> lambwiki_main_loop (add_other accum End_link)
        | '|'                                     -> lambwiki_main_loop (add_other accum Link_sep)
        | '&', Opt '#', Plus (alpha | digit), ';' -> lambwiki_main_loop (add_other accum (Entity (ltrim_lexbuf ~first:1 lexbuf)))
        | '[', numbers, ']'                       -> lambwiki_main_loop (add_other accum (Cite (get_labelref lexbuf)))
        | '(', numbers, ')'                       -> lambwiki_main_loop (add_other accum (See (get_labelref lexbuf)))
        | any                                     -> lambwiki_main_loop (add_plain accum (Sedlexing.Utf8.lexeme lexbuf))
        | _                                       -> assert false
    and lambwiki_raw_loop accum = match%sedlex lexbuf with
        | eol | eof   -> accum
        | "}}"        -> lambwiki_main_loop (add_other accum End_code)
        | any         -> lambwiki_raw_loop (add_plain accum (Sedlexing.Utf8.lexeme lexbuf))
        | _           -> assert false
    and markdown_main_loop accum = match%sedlex lexbuf with
        | eol | eof                               -> accum
        | escape, eol                             -> accum
        | escape, any                             -> markdown_main_loop (add_plain accum (Sedlexing.Utf8.sub_lexeme lexbuf 1 1))
        | "---"                                   -> markdown_main_loop (add_other accum (Entity "mdash"))
        | "--"                                    -> markdown_main_loop (add_other accum (Entity "ndash"))
        | "``"                                    -> markdown_main_loop (add_other accum (Entity "ldquo"))
        | "''"                                    -> markdown_main_loop (add_other accum (Entity "rdquo"))
        | "**"                                    -> markdown_main_loop (add_other accum Bold_mark)
        | "*"                                     -> markdown_main_loop (add_other accum Emph_mark)
        | "`"                                     -> markdown_raw_loop (add_other accum Begin_code)
        | "[["                                    -> link_loop markdown_main_loop (add_other accum Begin_link)
        | "]]"                                    -> markdown_main_loop (add_other accum End_link)
        | '|'                                     -> markdown_main_loop (add_other accum Link_sep)
        | '&', Opt '#', Plus (alpha | digit), ';' -> markdown_main_loop (add_other accum (Entity (ltrim_lexbuf ~first:1 lexbuf)))
        | '[', numbers, ']'                       -> markdown_main_loop (add_other accum (Cite (get_labelref lexbuf)))
        | '(', numbers, ')'                       -> markdown_main_loop (add_other accum (See (get_labelref lexbuf)))
        | any                                     -> markdown_main_loop (add_plain accum (Sedlexing.Utf8.lexeme lexbuf))
        | _                                       -> assert false
    and markdown_raw_loop accum = match%sedlex lexbuf with
        | eol | eof   -> accum
        | '`'         -> markdown_main_loop (add_other accum End_code)
        | any         -> markdown_raw_loop (add_plain accum (Sedlexing.Utf8.lexeme lexbuf))
        | _           -> assert false
    and link_loop main_loop accum = match%sedlex lexbuf with
        | eol | eof   -> accum
        | escape, eol -> accum
        | escape, any -> link_loop main_loop (add_plain accum (Sedlexing.Utf8.sub_lexeme lexbuf 1 1))
        | "]]"        -> main_loop (add_other accum End_link)
        | '|'         -> main_loop (add_other accum Link_sep)
        | any         -> link_loop main_loop (add_plain accum (Sedlexing.Utf8.lexeme lexbuf))
        | _           -> assert false in
    let postproc = function
        | `Plain buf -> Plain (Buffer.contents buf)
        | `Other x   -> x in
    let main_loop = match syntax with
        | `Lambwiki -> lambwiki_main_loop
        | `Markdown -> markdown_main_loop in
    main_loop [] |> List.rev_map postproc

let scan_literal terminator qprefix iprefix lexbuf =
    let style = ltrim_lexbuf ~first:3 lexbuf |> String.rstrip in
    let buf = Buffer.create 512 in
    let prefix = qprefix ^ iprefix in
    let rec loop nlines = match%sedlex lexbuf with
        | eof ->
            raise (Unterminated_literal terminator)
        | Star (Compl eol), (eol | eof) ->
            let str = String.rstrip (Sedlexing.Utf8.lexeme lexbuf) in
            if String.starts_with str prefix
            then begin
                let unprefixed = String.chop ~left:(String.length prefix) str in
                if unprefixed = terminator
                then
                    nlines
                else begin
                    Buffer.add_string buf unprefixed;
                    Buffer.add_char buf '\n';
                    loop (nlines+1)
                end
            end
            else
                raise (Bad_literal_prefix (prefix, str))
        | _ ->
            assert false in
    let nlines = loop 0 in
    (nlines, style, Buffer.contents buf)


(********************************************************************************)
(*  {1 Public functions and values}                                             *)
(********************************************************************************)

let next ~syntax lexbuf =
    let rec scan qprefix iprefix = match%sedlex lexbuf with
        | eof ->
            Eof
        | Star ('>', Opt blank) ->
            if qprefix = "" && iprefix = ""
            then scan (Sedlexing.Utf8.lexeme lexbuf) ""
            else raise Misaligned_quotation
        | Plus blank ->
            scan qprefix (Sedlexing.Utf8.lexeme lexbuf)
        | "{{{", Opt style, Star blank, (eol | eof) ->
            let (nlines, style, raw) = scan_literal "}}}" qprefix iprefix lexbuf in
            Regular (qprefix, iprefix, Literal (Source, nlines, style, raw))
        | "```", Opt style, Star blank, (eol | eof) ->
            let (nlines, style, raw) = scan_literal "```" qprefix iprefix lexbuf in
            Regular (qprefix, iprefix, Literal (Source, nlines, style, raw))
        | "(((", Opt style, Star blank, (eol | eof) ->
            let (nlines, style, raw) = scan_literal ")))" qprefix iprefix lexbuf in
            Regular (qprefix, iprefix, Literal (Verbatim, nlines, style, raw))
        | Plus '=', Star blank, (eol | eof) ->
            Regular (qprefix, iprefix, Rule Double)
        | Plus '-', Star blank, (eol | eof) ->
            Regular (qprefix, iprefix, Rule Single)
        | Plus '*', Star blank, (eol | eof) ->
            Regular (qprefix, iprefix, Rule Star)
        | (Plus '=' | Plus '#'), Star blank ->
            let level = Sedlexing.Utf8.lexeme lexbuf in
            Regular (qprefix, iprefix, Section (level, scan_text ~syntax lexbuf))
        | ('*' | '-'), Star blank ->
            Regular (qprefix, iprefix, Textual (Some Uli, scan_text ~syntax lexbuf))
        | number, ('.' | ')'), Star blank ->
            let oli = Sedlexing.Utf8.lexeme lexbuf |> String.rstrip in
            Regular (qprefix, iprefix, Textual (Some (Oli oli), scan_text ~syntax lexbuf))
        | number, Star ('.', number), Opt '.', Star blank ->
            let sec = Sedlexing.Utf8.lexeme lexbuf |> String.rstrip in
            Regular (qprefix, iprefix, Textual (Some (Sec sec), scan_text ~syntax lexbuf))
        | '[', number, ']', Star blank ->
            let label = get_labeldef lexbuf in
            Regular (qprefix, iprefix, Ghost (Sbib, label, scan_text ~syntax lexbuf))
        | '(', number, ')', Star blank ->
            let label = get_labeldef lexbuf in
            Regular (qprefix, iprefix, Ghost (Note, label, scan_text ~syntax lexbuf))
        | any ->
            Sedlexing.rollback lexbuf;
            Regular (qprefix, iprefix, Textual (None, scan_text ~syntax lexbuf))
        | _ ->
            assert false in
    scan "" ""

