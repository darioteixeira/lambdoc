module Ast = Lambdoc_reader_ast
module Readconv = Lambdoc_reader_readconv

open Lambdoc_prelude
open Lambdoc_document
open Valid
open Invalid
open Ast


(********************************************************************************)
(** {1 Private exceptions}                                                      *)
(********************************************************************************)

exception Value_error of Error.msg


(********************************************************************************)
(** {1 Private type definitions}                                                *)
(********************************************************************************)

type raw =
    | Unnamed of string
    | Named of string * string

type decl =
    | Classname_decl of classname
    | Lang_decl of Camlhighlight_core.lang option
    | Linenums_decl of bool
    | Width_decl of int option
    | Cols_decl of Tabular.colfmt array option
    | Cell_decl of Tabular.cellfmt option


(********************************************************************************)
(** {1 Public type definitions}                                                 *)
(********************************************************************************)

type _ handle =
    | Lang_hnd: Camlhighlight_core.lang option handle
    | Linenums_hnd: bool handle
    | Width_hnd: int option handle
    | Cols_hnd: Tabular.colfmt array option handle
    | Cell_hnd: Tabular.cellfmt option handle

type parsing = (raw * decl) list


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


let colfmt_of_char chr =
    let open Tabular in
    let (alignment, weight) = match chr with
        | 'c' -> (Center, Normal)
        | 'C' -> (Center, Strong)
        | 'l' -> (Left, Normal)
        | 'L' -> (Left, Strong)
        | 'r' -> (Right, Normal)
        | 'R' -> (Right, Strong)
        | 'j' -> (Justify, Normal)
        | 'J' -> (Justify, Strong)
        | x   -> invalid_arg ("Lambdoc_reader_style.colfmt_of_char: " ^ String.make 1 x) in
    Tabular.make_colfmt ~alignment ~weight


let cols_of_string key str =
    try Some (Array.init (String.length str) (fun i -> colfmt_of_char str.[i]))
    with _ -> raise (Value_error (Error.Invalid_style_bad_colsfmt (key, str)))


let cellfmt_of_string =
    let rex = Re.(compile (seq [bos; group (rep1 (rg '0' '9')); group (alt [rg 'a' 'z'; rg 'A' 'Z']); group (rep (set "^_")); eos])) in
    fun str ->
        let groups = Re.exec rex str in
        let colspan = int_of_string (Re.get groups 1) in
        let colfmt = colfmt_of_char (Re.get groups 2).[0] in
        let (overline, underline) = match Re.get groups 3 with
            | "^_" | "_^" -> (true, true)
            | "^"         -> (true, false)
            | "_"         -> (false, true)
            | _           -> (false, false)
        in Tabular.make_cellfmt ~colfmt ~colspan ~overline ~underline
            

let cell_of_string key str =
    try Some (cellfmt_of_string str)
    with _ -> raise (Value_error (Error.Invalid_style_bad_cellfmt (key, str)))

let decl_dict =
    [
    ("lang", fun v -> Lang_decl (lang_of_string "lang" v));
    ("nums", fun v -> Linenums_decl (boolean_of_string "nums" v));
    ("width", fun v -> Width_decl (numeric_of_string "width" ~low:1 ~high:100 v));
    ("cols", fun v -> Cols_decl (cols_of_string "cols" v));
    ("cell", fun v -> Cell_decl (cell_of_string "cell" v));
    ]


let raws_of_string =
    let rex = Re.(compile (seq [bos; group (rep1 (rg 'a' 'z')); char '='; group (rep1 any); eos])) in
    fun comm ->
        let conv (accum_raw, accum_msg) str =
            try
                let _ = String.index str '=' in
                begin try
                    let groups = Re.exec rex str in
                    let key = Re.get groups 1 in
                    let value = Re.get groups 2 in
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
                String.nsplit_by_char str ',' |>
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
    let matches: type a. a handle -> raw * decl -> a option = fun hnd (_, decl) -> match (hnd, decl) with
        | (Lang_hnd, Lang_decl x)         -> Some x
        | (Linenums_hnd, Linenums_decl x) -> Some x
        | (Width_hnd, Width_decl x)       -> Some x
        | (Cols_hnd, Cols_decl x)         -> Some x
        | (Cell_hnd, Cell_decl x)         -> Some x
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
    let rec split accum_parsing accum_classname = function
        | [] ->
            (accum_parsing, accum_classname)
        | ((_, decl) as hd) :: tl -> match decl with
            | Classname_decl x -> split accum_parsing (x :: accum_classname) tl
            | _                -> split (hd :: accum_parsing) accum_classname tl in
    let (raws, msgs) = raws_of_string comm in
    let (parsing, msgs) = List.fold_left make_parsing ([], msgs) raws in
    let (parsing, classnames) = split [] [] parsing in
    (classnames, ref parsing, List.rev msgs) (* We use List.rev because compiler expects errors in ascending order of line number *)


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

