module Lexer = Lambdoc_rlamblite_lexer
module Parser = Lambdoc_rlamblite_parser
module Tokenizer = Lambdoc_rlamblite_tokenizer

open Lexing


(********************************************************************************)
(*  {1 Type definitions}                                                        *)
(********************************************************************************)

type options = [ `Lambwiki | `Markdown ]


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
    let tokenizer = Tokenizer.make ?options ~linenum_offset str in
    try
        `Okay (menhir_with_sedlex Parser.document tokenizer)
    with exc ->
        let msg = match exc with
            | Parser.Error                     -> "Syntax error"
            | Tokenizer.Bad_indentation        -> "Bad indentation"
            | Tokenizer.Empty_tprefix _        -> "Empty textual prefix"
            | Tokenizer.Mismatched_rule _      -> "Mismatched rule"
            | Tokenizer.Mismatched_quotation _ -> "Mismatched quotation"
            | Tokenizer.Misplaced_numbered     -> "Misplaced numbered"
            | Tokenizer.Misaligned_section     -> "Misaligned section"
            | Tokenizer.Misaligned_listing     -> "Misaligned listing"
            | Lexer.Unterminated_literal _     -> "The block of raw text that starts on this line is not terminated"
            | Lexer.Bad_literal_prefix _       -> "In the block of text that starts on this line, all the lines must have the same whitespace prefix as the starting one"
            | Lexer.Misaligned_quotation       -> "Quotation block is misaligned"
            | exc                              -> raise exc
        in `Error [(Some (Tokenizer.get_position tokenizer).pos_lnum, None, msg)]

