(********************************************************************************)
(*  Lambdoc_rlamblite_tokenizer.mli
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module Parser = Lambdoc_rlamblite_parser
module Lexer = Lambdoc_rlamblite_lexer


(********************************************************************************)
(** {1 Exceptions}                                                              *)
(********************************************************************************)

exception Bad_indentation
exception Empty_tprefix of Lexer.tprefix
exception Mismatched_quotation of string * string
exception Mismatched_rule of int * Lexer.rule
exception Misplaced_numbered
exception Misaligned_section
exception Misaligned_listing


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type tokenizer


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val make: ?options:Lexer.syntax -> linenum_offset:int -> string -> tokenizer
val next_token: tokenizer -> Parser.token
val get_position: tokenizer -> Lexing.position

