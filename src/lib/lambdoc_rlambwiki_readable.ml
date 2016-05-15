(********************************************************************************)
(*  Lambdoc_rlambwiki_readable.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module Parser = Lambdoc_rlambwiki_parser
module Tokenizer = Lambdoc_rlambwiki_tokenizer

open Lexing


(********************************************************************************)
(*  {1 Type definitions}                                                        *)
(********************************************************************************)

type options = unit


(********************************************************************************)
(*  {1 Private functions and values}                                            *)
(********************************************************************************)

let menhir_with_sedlex menhir_parser tokenizer =
    let lexer_maker () =
        let ante_position = Tokenizer.get_position tokenizer in
        let token = Tokenizer.next_token tokenizer in
        let post_position = Tokenizer.get_position tokenizer in
        (token, ante_position, post_position) in
    let revised_parser = MenhirLib.Convert.Simplified.traditional2revised menhir_parser in
    revised_parser lexer_maker


(********************************************************************************)
(*  {1 Public functions and values}                                             *)
(********************************************************************************)

let ast_from_string ?options ~linenum_offset ~inline_extdefs ~block_extdefs str =
    let tokenizer = Tokenizer.make ~linenum_offset str in
    try
        `Okay (menhir_with_sedlex Parser.document tokenizer)
    with exc ->
        let msg = match exc with
            | Parser.Error -> "Syntax error"
            | exc -> raise exc
        in `Error [(Some (Tokenizer.get_position tokenizer).pos_lnum, None, msg)]

