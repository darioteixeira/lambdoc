module Context = Lambdoc_rlambtex_context
module Parser = Lambdoc_rlambtex_parser


(********************************************************************************)
(** {1 Public functors}                                                         *)
(********************************************************************************)

module Make: functor (C : Context.S) (P: module type of Parser.Make (C)) ->
sig
    type tokenizer 

    val make:
        linenum_offset:int ->
        inline_extdefs:Lambdoc_reader.Extension.extdef list ->
        block_extdefs:Lambdoc_reader.Extension.extdef list ->
        lexbuf:Sedlexing.lexbuf ->
        tokenizer

    val next_token: tokenizer -> P.token

    val get_position: tokenizer -> Lexing.position
end

