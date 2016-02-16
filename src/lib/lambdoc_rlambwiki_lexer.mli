(********************************************************************************)
(*  Lambdoc_rlambwiki_lexer.mli
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Lexers for Lambwiki reader.  We use Sedlex for handling the UTF-8 parsing.
*)


(********************************************************************************)
(*  {1 Type definitions}                                                        *)
(********************************************************************************)

type listing =
    | Ulist
    | Olist

type text =
    | Plain of string
    | Entity of string
    | Bold_mark
    | Emph_mark
    | Sup_mark
    | Sub_mark
    | Begin_caps | End_caps
    | Begin_mono | End_mono
    | Begin_link | End_link | Link_sep

type line =
    | Begin_sourceblk of string | End_sourceblk
    | Begin_verbatim of string | End_verbatim
    | Raw of string
    | Section of int * text list
    | Par of int * (listing * int) option * text list


(********************************************************************************)
(*  {1 Public functions and values}                                             *)
(********************************************************************************)

val source: Sedlexing.lexbuf -> line
val verbatim: Sedlexing.lexbuf -> line
val general: Sedlexing.lexbuf -> line

