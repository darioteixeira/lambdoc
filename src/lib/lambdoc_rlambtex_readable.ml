module Context = Lambdoc_rlambtex_context
module Parser = Lambdoc_rlambtex_parser
module Tokenizer = Lambdoc_rlambtex_tokenizer


(********************************************************************************)
(*  {1 Type definitions}                                                        *)
(********************************************************************************)

type options = unit


(********************************************************************************)
(*  {1 Public functions and values}                                             *)
(********************************************************************************)

let ast_from_string ?options ~linenum_offset ~inline_extdefs ~block_extdefs str =
    let module C = Context.Make (struct end) in
    let module P = Parser.Make (C) in
    let module T = Tokenizer.Make (C) (P) in
    let lexbuf = Sedlexing.Utf8.from_string str in
    let tokenizer = T.make ~linenum_offset ~inline_extdefs ~block_extdefs ~lexbuf in
    let lexer_maker () =
        let ante_position = T.get_position tokenizer in
        let token = T.next_token tokenizer in
        let post_position = T.get_position tokenizer in
        (token, ante_position, post_position) in
    try
        `Okay (MenhirLib.Convert.Simplified.traditional2revised P.main lexer_maker)
    with exc ->
        let position = T.get_position tokenizer in
        let msg = match exc with
            | Utf8.MalFormed ->
                "Malformed UTF-8 sequence"
            | P.Error ->
                "Syntax error"
            | exc ->
                raise exc
        in `Error [(Some position.pos_lnum, None, msg)]

