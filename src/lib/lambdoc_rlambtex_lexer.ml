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

type accum =
    {
    buffer: Buffer.t;
    mutable nlines: int;
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

let make_accum () =
    {
    buffer = Buffer.create 80;
    nlines = 0;
    }

let return ?lexbuf accum current_lexeme =
    let current_nlines = match lexbuf with
        | Some lexbuf -> count_newlines lexbuf
        | None        -> 0 in
    let previous =
        if Buffer.length accum.buffer = 0
        then None
        else Some (Text (Buffer.contents accum.buffer), accum.nlines) in
    let current = (current_lexeme, current_nlines) in
    {previous; current;}


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let general lexbuf =
    let accum = make_accum () in
    let rec loop () = match%sedlex lexbuf with
        | begin_env, Star space, Opt newline ->
            return ~lexbuf accum (Begin_env (whole_lexbuf lexbuf |> String.trim))
        | end_env, Star space, Opt newline ->
            return ~lexbuf accum (End_env (whole_lexbuf lexbuf |> String.trim))
        | begin_mathtexinl ->
            return accum Begin_mathtexinl
        | end_mathtexinl ->
            return accum End_mathtexinl
        | begin_mathmlinl ->
            return accum Begin_mathmlinl
        | end_mathmlinl ->
            return accum End_mathmlinl
        | open_marker ->
            return accum Open
        | close_marker ->
            return accum Close
        | simple ->
            return accum (Simple (whole_lexbuf lexbuf))
        | cell_mark ->
            return ~lexbuf accum (Cell_mark (whole_lexbuf lexbuf |> String.trim))
        | row_end ->
            return ~lexbuf accum Row_end
        | Opt eol, eof ->
            return accum Eof
        | parbreak ->
            return ~lexbuf accum Par_break
        | Plus space | eol ->
            return ~lexbuf accum Space
        | entity ->
            return accum (Entity (trim_lexbuf ~left:1 ~right:1 lexbuf))
        | ndash ->
            return accum (Entity "ndash")
        | mdash ->
            return accum (Entity "mdash")
        | ldquo ->
            return accum (Entity "ldquo")
        | rdquo ->
            return accum (Entity "rdquo")
        | escape, newline ->
            accum.nlines <- accum.nlines + 1;
            loop ()
        | escape, any ->
            Buffer.add_string accum.buffer (sub_lexbuf ~pos:1 ~len:1 lexbuf);
            loop ()
        | any ->
            Buffer.add_string accum.buffer (whole_lexbuf lexbuf);
            loop ()
        | _ ->
            assert false
    in loop ()


let raw lexbuf =
    let accum = make_accum () in
    let rec loop () = match%sedlex lexbuf with
        | close_marker ->
            return accum Close
        | eof ->
            return accum Eof
        | newline ->
            Buffer.add_char accum.buffer '\n';
            accum.nlines <- accum.nlines + 1;
            loop ()
        | escape, newline ->
            accum.nlines <- accum.nlines + 1;
            loop ()
        | escape, any ->
            Buffer.add_string accum.buffer (sub_lexbuf ~pos:1 ~len:1 lexbuf);
            loop ()
        | any ->
            Buffer.add_string accum.buffer (whole_lexbuf lexbuf);
            loop ()
        | _ ->
            assert false
    in loop ()


let mathtexinl lexbuf =
    let accum = make_accum () in
    let rec loop () = match%sedlex lexbuf with
        | end_mathtexinl ->
            return accum End_mathtexinl
        | eof ->
            return accum Eof
        | newline ->
            Buffer.add_char accum.buffer '\n';
            accum.nlines <- accum.nlines + 1;
            loop ()
        | any ->
            Buffer.add_string accum.buffer (whole_lexbuf lexbuf);
            loop ()
        | _ ->
            assert false
    in loop ()


let mathmlinl lexbuf =
    let accum = make_accum () in
    let rec loop () = match%sedlex lexbuf with
        | end_mathmlinl ->
            return accum End_mathmlinl
        | eof ->
            return accum Eof
        | newline ->
            Buffer.add_char accum.buffer '\n';
            accum.nlines <- accum.nlines + 1;
            loop ()
        | any ->
            Buffer.add_string accum.buffer (whole_lexbuf lexbuf);
            loop ()
        | _ ->
            assert false
    in loop ()


let literal terminator lexbuf =
    let accum = make_accum () in
    let rec loop () = match%sedlex lexbuf with
        | Opt newline, end_env, Star space, Opt newline ->
            let trimmed = whole_lexbuf lexbuf |> String.trim in
            if (String.slice ~first:5 ~last:(-1) trimmed) = terminator
            then
                return ~lexbuf accum (End_env trimmed)
            else begin
                accum.nlines <- accum.nlines + count_newlines lexbuf;
                Buffer.add_string accum.buffer (whole_lexbuf lexbuf);
                loop ()
            end
        | eof ->
            return accum Eof
        | newline ->
            Buffer.add_char accum.buffer '\n';
            accum.nlines <- accum.nlines + 1;
            loop ()
        | any ->
            Buffer.add_string accum.buffer (whole_lexbuf lexbuf);
            loop ()
        | _ ->
            assert false in
    loop ()

