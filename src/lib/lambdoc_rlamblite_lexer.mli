(********************************************************************************)
(*  {1 Exceptions}                                                              *)
(********************************************************************************)

exception Unterminated_literal of string
exception Bad_literal_prefix of string * string
exception Misaligned_quotation


(********************************************************************************)
(*  {1 Type definitions}                                                        *)
(********************************************************************************)

type inline =
    | Plain of string
    | Entity of string
    | Cite of string list
    | See of string list
    | Mathtex_mark
    | Bold_mark
    | Emph_mark
    | Sup_mark
    | Sub_mark
    | Ins_mark
    | Del_mark
    | Begin_mono | End_mono
    | Begin_code | End_code
    | Begin_link | End_link | Link_sep

type rule = Single | Double | Star

type literal = Mathtex | Source | Verbatim

type ghost = Sbib | Note

type tprefix =
    | Sec of string
    | Oli of string
    | Uli

type regular =
    | Rule of rule
    | Literal of literal * int * string * string
    | Ghost of ghost * string * inline list
    | Section of string * inline list
    | Textual of tprefix option * inline list

type lexeme =
    | Eof
    | Regular of string * string * regular

type syntax = [ `Lambwiki | `Markdown ]


(********************************************************************************)
(*  {1 Public functions and values}                                             *)
(********************************************************************************)

val next: syntax:syntax -> Sedlexing.lexbuf -> lexeme

