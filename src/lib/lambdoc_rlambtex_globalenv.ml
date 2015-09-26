(********************************************************************************)
(*  Lambdoc_rlambtex_globalenv.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)


(********************************************************************************)
(** {1 Exceptions}                                                              *)
(********************************************************************************)

exception Pop_mismatch of string * string


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type scanner =
    | Scanner_general
    | Scanner_raw
    | Scanner_mathtex_inl
    | Scanner_mathml_inl
    | Scanner_tabular
    | Scanner_literal of string


type t = string option * scanner


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

let string_of_name = function
    | Some n -> n
    | None   -> "(none)"

let history =
    let stack = Stack.create () in
    Stack.push (None, Scanner_general) stack;
    stack

let push env =
    Stack.push env history

let pop new_name =
    let (old_name, _) = Stack.pop history in
    if new_name <> old_name
    then raise (Pop_mismatch (string_of_name new_name, string_of_name old_name))

let get_scanner () =
    let (_, scanner) = Stack.top history in
    scanner

