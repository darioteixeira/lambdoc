(********************************************************************************)
(*  Lambdoc_rlambtex_lexer.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module String = BatString


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type buffer =
    {
    lexbuf: Sedlexing.lexbuf;
    txtbuf: Buffer.t;
    }

type lexeme =
    | Begin_env of string
    | End_env of string
    | Begin_mathtexinl
    | End_mathtexinl
    | Begin_mathmlinl
    | End_mathmlinl
    | Open
    | Close
    | Simple of string
    | Cell_mark of string
    | Row_end
    | Par_break
    | Space
    | Text of string
    | Entity of string
    | Eof

type pair = lexeme * int

type outcome =
    {
    previous: pair option;
    current: pair;
    }


(********************************************************************************)
(** {1 Regular expressions used in the various lexers}                          *)
(********************************************************************************)

let open_marker = [%sedlex.regexp? '{']
let close_marker = [%sedlex.regexp? '}']

let begin_mathtexinl = [%sedlex.regexp? "[$"]
let end_mathtexinl = [%sedlex.regexp? "$]"]
let begin_mathmlinl = [%sedlex.regexp? "<$"]
let end_mathmlinl = [%sedlex.regexp? "$>"]

let lower = [%sedlex.regexp? 'a' .. 'z']
let alpha = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']
let deci = [%sedlex.regexp? '0' .. '9']
let ident = [%sedlex.regexp? lower, Star (lower | deci | '_')]

let label = [%sedlex.regexp? '[', Star (Compl (Chars "]()<>{}")), ']']
let order = [%sedlex.regexp? '(', Star (Compl (Chars ")[]<>{}")), ')']
let style = [%sedlex.regexp? '<', Star (Compl (Chars ">[](){}")), '>']
let optional = [%sedlex.regexp? Star (label | order | style)]
let primary = [%sedlex.regexp? '{', ident, '}']

let simple = [%sedlex.regexp? '\\', ident, optional]
let begin_env = [%sedlex.regexp? "\\begin", optional, primary]
let end_env = [%sedlex.regexp? "\\end", primary]

let newline = [%sedlex.regexp? "\r\n" | '\n']
let space = [%sedlex.regexp? Chars " \t"]
let escape = [%sedlex.regexp? '\\']
let eol = [%sedlex.regexp? Star space, newline, Star space]
let parbreak = [%sedlex.regexp? eol, Plus eol]

let cell_mark = [%sedlex.regexp? Star space, '|', optional, Star space]
let row_end = [%sedlex.regexp? Star space, '|', Star space, newline]

let entity = [%sedlex.regexp? '&', Opt '#', Plus (alpha | deci), ';']
let ndash = [%sedlex.regexp? "--"]
let mdash = [%sedlex.regexp? "---"]
let ldquo = [%sedlex.regexp? "``"]
let rdquo = [%sedlex.regexp? "''"]


(********************************************************************************)
(** {1 Auxiliary functions}                                                     *)
(********************************************************************************)

let whole_lexbuf lexbuf =
    Sedlexing.Utf8.lexeme lexbuf

let trim_lexbuf ?(left = 0) ?(right = 0) lexbuf =
    Sedlexing.Utf8.sub_lexeme lexbuf left (Sedlexing.lexeme_length lexbuf - left - right)

let sub_lexbuf ~pos ~len lexbuf =
    Sedlexing.Utf8.sub_lexeme lexbuf pos len

let count_newlines lexbuf =
    let adder acc el = if el = 0x0a then acc+1 else acc in
    Array.fold_left adder 0 (Sedlexing.lexeme lexbuf)

let return ?lexbuf nlines txtbuf current_lexeme =
    let current_nlines = match lexbuf with
        | Some lexbuf -> count_newlines lexbuf
        | None        -> 0 in
    let previous =
        if Buffer.length txtbuf = 0
        then
            None
        else begin
            let text = Buffer.contents txtbuf in
            Buffer.clear txtbuf;
            Some (Text text, nlines)
        end in
    let current = (current_lexeme, current_nlines) in
    {previous; current;}


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let make_buffer lexbuf =
    {lexbuf; txtbuf = Buffer.create 80}

let general {lexbuf; txtbuf} =
    let rec loop nlines = match%sedlex lexbuf with
        | begin_env, Star space, Opt newline ->
            return ~lexbuf nlines txtbuf (Begin_env (whole_lexbuf lexbuf |> String.trim))
        | end_env, Star space, Opt newline ->
            return ~lexbuf nlines txtbuf (End_env (whole_lexbuf lexbuf |> String.trim))
        | begin_mathtexinl ->
            return nlines txtbuf Begin_mathtexinl
        | end_mathtexinl ->
            return nlines txtbuf End_mathtexinl
        | begin_mathmlinl ->
            return nlines txtbuf Begin_mathmlinl
        | end_mathmlinl ->
            return nlines txtbuf End_mathmlinl
        | open_marker ->
            return nlines txtbuf Open
        | close_marker ->
            return nlines txtbuf Close
        | simple ->
            return nlines txtbuf (Simple (whole_lexbuf lexbuf))
        | cell_mark ->
            return ~lexbuf nlines txtbuf (Cell_mark (whole_lexbuf lexbuf |> String.trim))
        | row_end ->
            return ~lexbuf nlines txtbuf Row_end
        | Opt eol, eof ->
            return nlines txtbuf Eof
        | parbreak ->
            return ~lexbuf nlines txtbuf Par_break
        | Plus space | eol ->
            return ~lexbuf nlines txtbuf Space
        | entity ->
            return nlines txtbuf (Entity (trim_lexbuf ~left:1 ~right:1 lexbuf))
        | ndash ->
            return nlines txtbuf (Entity "ndash")
        | mdash ->
            return nlines txtbuf (Entity "mdash")
        | ldquo ->
            return nlines txtbuf (Entity "ldquo")
        | rdquo ->
            return nlines txtbuf (Entity "rdquo")
        | escape, newline ->
            loop (nlines+1)
        | escape, any ->
            Buffer.add_string txtbuf (sub_lexbuf ~pos:1 ~len:1 lexbuf);
            loop nlines
        | any ->
            Buffer.add_string txtbuf (whole_lexbuf lexbuf);
            loop nlines
        | _ ->
            assert false
    in loop 0


let raw {lexbuf; txtbuf} =
    let rec loop nlines = match%sedlex lexbuf with
        | close_marker ->
            return nlines txtbuf Close
        | eof ->
            return nlines txtbuf Eof
        | newline ->
            Buffer.add_char txtbuf '\n';
            loop (nlines+1)
        | escape, newline ->
            loop (nlines+1)
        | escape, any ->
            Buffer.add_string txtbuf (sub_lexbuf ~pos:1 ~len:1 lexbuf);
            loop nlines
        | any ->
            Buffer.add_string txtbuf (whole_lexbuf lexbuf);
            loop nlines
        | _ ->
            assert false
    in loop 0


let mathtexinl {lexbuf; txtbuf} =
    let rec loop nlines = match%sedlex lexbuf with
        | end_mathtexinl ->
            return nlines txtbuf End_mathtexinl
        | eof ->
            return nlines txtbuf Eof
        | newline ->
            Buffer.add_char txtbuf '\n';
            loop (nlines+1)
        | any ->
            Buffer.add_string txtbuf (whole_lexbuf lexbuf);
            loop nlines
        | _ ->
            assert false
    in loop 0


let mathmlinl {lexbuf; txtbuf} =
    let rec loop nlines = match%sedlex lexbuf with
        | end_mathmlinl ->
            return nlines txtbuf End_mathmlinl
        | eof ->
            return nlines txtbuf Eof
        | newline ->
            Buffer.add_char txtbuf '\n';
            loop (nlines+1)
        | any ->
            Buffer.add_string txtbuf (whole_lexbuf lexbuf);
            loop nlines
        | _ ->
            assert false
    in loop 0


let literal terminator {lexbuf; txtbuf} =
    let rec loop nlines = match%sedlex lexbuf with
        | Opt newline, end_env, Star space, Opt newline ->
            let trimmed = whole_lexbuf lexbuf |> String.trim in
            if (String.slice ~first:5 ~last:(-1) trimmed) = terminator
            then
                return ~lexbuf nlines txtbuf (End_env trimmed)
            else begin
                Buffer.add_string txtbuf (whole_lexbuf lexbuf);
                loop (nlines + count_newlines lexbuf)
            end
        | eof ->
            return nlines txtbuf Eof
        | newline ->
            Buffer.add_char txtbuf '\n';
            loop (nlines+1)
        | any ->
            Buffer.add_string txtbuf (whole_lexbuf lexbuf);
            loop nlines
        | _ ->
            assert false in
    loop 0

