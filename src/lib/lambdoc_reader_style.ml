(********************************************************************************)
(*  Lambdoc_reader_style.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module Array = BatArray
module List = BatList
module String = BatString
module Ast = Lambdoc_reader_ast
module Readconv = Lambdoc_reader_readconv

open Lambdoc_core
open Basic
open Ast


(********************************************************************************)
(** {1 Private exceptions}                                                      *)
(********************************************************************************)

exception Value_error of Error.msg_t


(********************************************************************************)
(** {1 Private type definitions}                                                *)
(********************************************************************************)

type raw_t =
    | Unnamed of string
    | Named of string * string

type decl_t =
    | Classname_decl of Classname.t
    | Lang_decl of Camlhighlight_core.lang_t option
    | Linenums_decl of bool
    | Width_decl of int option


(********************************************************************************)
(** {1 Public type definitions}                                                 *)
(********************************************************************************)

type _ handle_t =
    | Lang_hnd: Camlhighlight_core.lang_t option handle_t
    | Linenums_hnd: bool handle_t
    | Width_hnd: int option handle_t

type parsing_t = (raw_t * decl_t) list


(********************************************************************************)
(** {1 Private functions and values}                                            *)
(********************************************************************************)

let lang_of_string key = function
    | "none" ->
        None
    | x ->
        if Camlhighlight_parser.is_available_lang x
        then Some x
        else raise (Value_error (Error.Invalid_style_bad_lang (key, x)))


let boolean_of_string key = function
    | "true" | "on" | "yes"  -> true
    | "false" | "off" | "no" -> false
    | x                      -> raise (Value_error (Error.Invalid_style_bad_boolean (key, x)))


let numeric_of_string key ~low ~high = function
    | "none" ->
        None
    | x ->
        let exc = Value_error (Error.Invalid_style_bad_numeric (key, x, low, high)) in
        let num =
            try int_of_string x
            with Failure "int_of_string" -> raise exc in
        if (num >= low) && (num <= high)
        then Some num
        else raise exc


let decl_dict =
    [
    ("lang", fun v -> Lang_decl (lang_of_string "lang" v));
    ("nums", fun v -> Linenums_decl (boolean_of_string "nums" v));
    ("width", fun v -> Width_decl (numeric_of_string "width" ~low:1 ~high:100 v));
    ]


let raws_of_string =
    let kv_rex = Pcre.regexp "^(?<key>[a-z]+)=(?<value>.+)$" in
    fun comm ->
        let conv (accum_raw, accum_msg) str =
            try
                let _ = String.index str '=' in
                begin try
                    let subs = Pcre.exec ~rex:kv_rex str in
                    let key = Pcre.get_named_substring kv_rex "key" subs in
                    let value = Pcre.get_named_substring kv_rex "value" subs in
                    (Named (key, value) :: accum_raw, accum_msg)
                with Not_found ->
                    let msg = Error.Invalid_style_bad_keyvalue str in
                    (accum_raw, msg :: accum_msg)
                end
            with Not_found ->
                if Readconv.Identifier_input.matches_classname str
                then
                    (Unnamed str :: accum_raw, accum_msg)
                else
                    let msg = Error.Invalid_style_bad_classname str in
                    (accum_raw, msg :: accum_msg) in
        match comm.comm_style with
            | Some str ->
                String.nsplit str "," |>
                List.map String.strip |>
                List.fold_left conv ([], [])
            | None ->
                ([], [])


let decl_of_raw = function
    | Unnamed str ->
        `Okay (Classname_decl str)
    | Named (key, value) ->
        try
            let maker = List.assoc key decl_dict in
            `Okay (maker value)
        with
            | Not_found   -> `Error (Error.Invalid_style_unknown_keyvalue (key, value))
            | Value_error msg -> `Error msg


let find_decl hnd parsing =
    let matches: type a. a handle_t -> raw_t * decl_t -> a option = fun hnd (_, decl) -> match (hnd, decl) with
        | (Lang_hnd, Lang_decl x)         -> Some x
        | (Linenums_hnd, Linenums_decl x) -> Some x
        | (Width_hnd, Width_decl x)       -> Some x
        | _                               -> None in
    let rec finder accum = function
        | [] ->
            (None, accum)
        | hd :: tl ->
            match matches hnd hd with
                | Some x -> (Some x, accum @ tl)
                | None   -> finder (hd :: accum) tl in
    finder [] parsing


(********************************************************************************)
(** {2 Public functions and values}                                             *)
(********************************************************************************)

let parse comm =
    let make_parsing (accum_parsing, accum_msg) raw = match decl_of_raw raw with
        | `Okay decl -> ((raw, decl) :: accum_parsing, accum_msg)
        | `Error msg -> (accum_parsing, msg :: accum_msg) in
    let rec split accum_parsing accum_attr = function
        | [] ->
            (accum_parsing, accum_attr)
        | ((_, decl) as hd) :: tl -> match decl with
            | Classname_decl x -> split accum_parsing (x :: accum_attr) tl
            | _                -> split (hd :: accum_parsing) accum_attr tl in
    let (raws, msgs) = raws_of_string comm in
    let (parsing, msgs) = List.fold_left make_parsing ([], msgs) raws in
    let (parsing, attrs) = split [] [] parsing in
    (attrs, ref parsing, List.rev msgs) (* We use List.rev because compiler expects errors in ascending order of line number *)


let consume1 parsing_ref (hnd, default) =
    let (res, vs) = match find_decl hnd !parsing_ref with
        | (Some x, vs) -> (x, vs)
        | (None, vs)   -> (default, vs) in
    parsing_ref := vs;
    res


let consume2 parsing_ref (hnd1, default1) (hnd2, default2) =
    let (res1, vs) = match find_decl hnd1 !parsing_ref with
        | (Some x, vs) -> (x, vs)
        | (None, vs)   -> (default1, vs) in
    let (res2, vs) = match find_decl hnd2 vs with
        | (Some x, vs) -> (x, vs)
        | (None, vs)   -> (default2, vs) in
    parsing_ref := vs;
    (res1, res2)


let dispose comm parsing_ref = match !parsing_ref with
    | [] ->
        []
    | xs ->
        let aux (raw, _) = match raw with
            | Named (key, value) -> Error.Invalid_style_misplaced_keyvalue (key, value)
            | Unnamed _          -> assert false in
        List.rev_map aux xs

