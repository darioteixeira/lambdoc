(********************************************************************************)
(*  Lambdoc_rlambtex_lexer.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_prelude


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
    | Simple of string
    | Text of string
    | Entity of string
    | Cell_mark of string
    | Row_end
    | Mathtex_op
    | Open
    | Close
    | Eop
    | Eof

type triple = lexeme * int * int

type outcome =
    {
    previous: triple option;
    current: triple;
    }

type context = Block | Inline


(********************************************************************************)
(** {1 Regular expressions}                                                     *)
(********************************************************************************)

let open_marker = [%sedlex.regexp? '{']
let close_marker = [%sedlex.regexp? '}']

let mathtex_op = [%sedlex.regexp? "$$"]

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
let eop = [%sedlex.regexp? eol, Plus eol]

let cell_mark = [%sedlex.regexp? Star space, '|', optional, Star space]
let row_end = [%sedlex.regexp? Star space, '|', Star space, newline]

let entity = [%sedlex.regexp? '&', Opt '#', Plus (alpha | deci), ';']
let ndash = [%sedlex.regexp? "--"]
let mdash = [%sedlex.regexp? "---"]
let ldquo = [%sedlex.regexp? "``"]
let rdquo = [%sedlex.regexp? "''"]


(********************************************************************************)
(** {1 Private functions and values}                                            *)
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

let return ?lexbuf before during txtbuf current_lexeme =
    let current_during = match lexbuf with
        | Some lexbuf -> count_newlines lexbuf
        | None        -> 0 in
    let (previous, current_before) =
        if Buffer.length txtbuf = 0
        then
            (None, before)
        else begin
            let text = Buffer.contents txtbuf in
            Buffer.clear txtbuf;
            (Some (Text text, before, during), 0)
        end in
    let current = (current_lexeme, current_before, current_during) in
    {previous; current;}

let general ~context {lexbuf; txtbuf} =
    let rec loop before during = match%sedlex lexbuf with
        | begin_env ->
            return before during txtbuf (Begin_env (whole_lexbuf lexbuf))
        | end_env ->
            return before during txtbuf (End_env (whole_lexbuf lexbuf))
        | mathtex_op ->
            return before during txtbuf Mathtex_op
        | open_marker ->
            return before during txtbuf Open
        | close_marker ->
            return before during txtbuf Close
        | simple ->
            return before during txtbuf (Simple (whole_lexbuf lexbuf))
        | cell_mark ->
            return ~lexbuf before during txtbuf (Cell_mark (whole_lexbuf lexbuf |> String.trim))
        | row_end ->
            return ~lexbuf before during txtbuf Row_end
        | eof ->
            return before during txtbuf Eof
        | eop ->
            return ~lexbuf before during txtbuf Eop
        | Plus space | eol ->
            (if Buffer.length txtbuf <> 0 || context = Inline then Buffer.add_char txtbuf ' ');
            if Buffer.length txtbuf <> 0
            then loop before (during + count_newlines lexbuf)
            else loop (before + count_newlines lexbuf) during
        | entity ->
            return before during txtbuf (Entity (trim_lexbuf ~left:1 ~right:1 lexbuf))
        | ndash ->
            return before during txtbuf (Entity "ndash")
        | mdash ->
            return before during txtbuf (Entity "mdash")
        | ldquo ->
            return before during txtbuf (Entity "ldquo")
        | rdquo ->
            return before during txtbuf (Entity "rdquo")
        | escape, newline ->
            if Buffer.length txtbuf <> 0
            then loop before (during+1)
            else loop (before+1) during
        | escape, any ->
            Buffer.add_string txtbuf (sub_lexbuf ~pos:1 ~len:1 lexbuf);
            loop before during
        | any ->
            Buffer.add_string txtbuf (whole_lexbuf lexbuf);
            loop before during
        | _ ->
            assert false
    in loop 0 0


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let make_buffer lexbuf =
    {lexbuf; txtbuf = Buffer.create 80}

let block buffer =
    general ~context:Block buffer

let inline buffer =
    general ~context:Inline buffer

let raw {lexbuf; txtbuf} =
    let rec loop during = match%sedlex lexbuf with
        | close_marker ->
            return 0 during txtbuf Close
        | eof ->
            return 0 during txtbuf Eof
        | newline ->
            Buffer.add_char txtbuf '\n';
            loop (during+1)
        | escape, newline ->
            loop (during+1)
        | escape, any ->
            Buffer.add_string txtbuf (sub_lexbuf ~pos:1 ~len:1 lexbuf);
            loop during
        | any ->
            Buffer.add_string txtbuf (whole_lexbuf lexbuf);
            loop during
        | _ ->
            assert false
    in loop 0

let mathtex_inl {lexbuf; txtbuf} =
    let rec loop during = match%sedlex lexbuf with
        | mathtex_op ->
            return 0 during txtbuf Mathtex_op
        | eof ->
            return 0 during txtbuf Eof
        | newline ->
            Buffer.add_char txtbuf '\n';
            loop (during+1)
        | any ->
            Buffer.add_string txtbuf (whole_lexbuf lexbuf);
            loop during
        | _ ->
            assert false
    in loop 0

let literal terminator {lexbuf; txtbuf} =
    let rec loop during = match%sedlex lexbuf with
        | end_env ->
            let content = whole_lexbuf lexbuf in
            if (String.chop ~left:5 ~right:1 content) = terminator
            then
                return 0 during txtbuf (End_env content)
            else begin
                Buffer.add_string txtbuf content;
                loop during
            end
        | eof ->
            return 0 during txtbuf Eof
        | newline ->
            Buffer.add_char txtbuf '\n';
            loop (during+1)
        | any ->
            Buffer.add_string txtbuf (whole_lexbuf lexbuf);
            loop during
        | _ ->
            assert false in
    loop 0

