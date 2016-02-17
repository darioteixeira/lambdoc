(********************************************************************************)
(*  Lambdoc_rlambtex_tokenizer.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module String = BatString
module Context = Lambdoc_rlambtex_context
module Parser = Lambdoc_rlambtex_parser
module Lexer = Lambdoc_rlambtex_lexer

open Lambdoc_core
open Lambdoc_reader
open Ast
open Extension
open Lexing


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type trigger = Blk | Inl


(********************************************************************************)
(*  {1 Auxiliary regular expressions}                                           *)
(********************************************************************************)

let pat_space = Re.set " \t"
let pat_lower = Re.rg 'a' 'z'
let pat_digit = Re.rg '0' '9'
let pat_ident = Re.(seq [pat_lower; rep (alt [pat_lower; pat_digit; char '_'])])
let pat_env = Re.(seq [char '\\'; alt [str "begin"; str "end"]])
let pat_simple = Re.(seq [char '\\'; group pat_ident])
let pat_primary = Re.(seq [char '{'; group pat_ident; char '}'])

let pat_label = Re.(seq [char '['; group (rep (compl [set "]()<>{}"])); char ']'])
let pat_order = Re.(seq [char '('; group (rep (compl [set ")[]<>{}"])); char ')'])
let pat_style = Re.(seq [char '<'; group (rep (compl [set ">[](){}"])); char '>'])
let pat_optional = Re.(rep (alt [pat_label; pat_order; pat_style]))

let rex_env = Re.(compile (seq [bos; pat_env; pat_optional; pat_primary; eos]))
let rex_simple = Re.(compile (seq [bos; pat_simple; pat_optional; eos]))
let rex_cell_mark = Re.(compile (seq [bos; char '|'; pat_optional; eos]))


(********************************************************************************)
(** {1 Public functors}                                                         *)
(********************************************************************************)

module Make (C: Context.S) (P: module type of Parser.Make (C)) =
struct
    open Lexer
    open P

    type tokenizer =
        {
        inline_extdefs: Extension.extdef list;
        sim_block_extdefs: Extension.extdef list;
        env_block_extdefs: Extension.extdef list;
        buffer: Lexer.buffer;
        mutable position: Lexing.position;
        mutable waiting_lexeme: Lexer.triple option;
        mutable waiting_token: P.token option;
        mutable literal: string option;
        }

    let build_command tokenizer tag groups group_index =
        let get_group num =
            let num = group_index + num in
            if Re.test groups num
            then Some (Re.get groups num)
            else None
        in  {
            comm_tag = Some tag;
            comm_label = get_group 0;
            comm_order = get_group 1;
            comm_style = get_group 2;
            comm_linenum = tokenizer.position.pos_lnum;
            comm_originator = Attr.Source;
            }

    let build_op tokenizer =
        {
        comm_tag = None;
        comm_label = None;
        comm_order = None;
        comm_style = None;
        comm_linenum = tokenizer.position.pos_lnum;
        comm_originator = Attr.Source;
        }

    let is_env (_, syntax) = match syntax with
        | Syn_lit
        | Syn_frag -> true
        | Syn_empty
        | Syn_seq
        | Syn_raw _
        | Syn_raw_raw _
        | Syn_raw_seq _
        | Syn_raw_seqopt _ -> false

    let simple_command tokenizer raw_comm =
        let groups = Re.exec rex_simple raw_comm in
        let simple = Re.get groups 1 in
        let command = build_command tokenizer ("\\" ^ simple) groups 2 in
        match simple with
            | "br"                    -> (Some Inl, LINEBREAK command)
            | "glyph"                 -> (Some Inl, GLYPH command)
            | "code"                  -> (Some Inl, CODE command)
            | "bold" | "strong" | "b" -> (Some Inl, BOLD command)
            | "emph" | "em" | "i"     -> (Some Inl, EMPH command)
            | "mono" | "tt"           -> (Some Inl, MONO command)
            | "caps"                  -> (Some Inl, CAPS command)
            | "ins"                   -> (Some Inl, INS command)
            | "del"                   -> (Some Inl, DEL command)
            | "sup"                   -> (Some Inl, SUP command)
            | "sub"                   -> (Some Inl, SUB command)
            | "mbox"                  -> (Some Inl, MBOX command)
            | "span"                  -> (Some Inl, SPAN command)
            | "link" | "a"            -> (Some Inl, LINK command)
            | "see"                   -> (Some Inl, SEE command)
            | "cite"                  -> (Some Inl, CITE command)
            | "dref"                  -> (Some Inl, DREF command)
            | "sref"                  -> (Some Inl, SREF command)
            | "mref"                  -> (Some Inl, MREF command)
            | "paragraph" | "p"       -> (Some Blk, PARAGRAPH command)
            | "picture"               -> (Some Blk, PICTURE command)
            | "part"                  -> (Some Blk, PART command)
            | "appendix"              -> (Some Blk, APPENDIX command)
            | "h1" | "section"        -> (Some Blk, SECTION (command, 1))
            | "h2" | "subsection"     -> (Some Blk, SECTION (command, 2))
            | "h3" | "subsubsection"  -> (Some Blk, SECTION (command, 3))
            | "h4"                    -> (Some Blk, SECTION (command, 4))
            | "h5"                    -> (Some Blk, SECTION (command, 5))
            | "h6"                    -> (Some Blk, SECTION (command, 6))
            | "bibliography"          -> (Some Blk, BIBLIOGRAPHY command)
            | "notes"                 -> (Some Blk, NOTES command)
            | "toc"                   -> (Some Blk, TOC command)
            | "title"                 -> (Some Blk, TITLE (command, 1))
            | "subtitle"              -> (Some Blk, TITLE (command, 2))
            | "rule" | "hr"           -> (Some Blk, RULE command)
            | "newmacro"              -> (Some Blk, MACRODEF command)
            | "newboxout"             -> (Some Blk, BOXOUTDEF command)
            | "newtheorem"            -> (Some Blk, THEOREMDEF command)
            | "item" | "li"           -> (Some Blk, ITEM command)
            | "question"              -> (Some Blk, QUESTION command)
            | "rquestion"             -> (Some Blk, RQUESTION command)
            | "answer"                -> (Some Blk, ANSWER command)
            | "ranswer"               -> (Some Blk, RANSWER command)
            | "head"                  -> (None, THEAD command)
            | "foot"                  -> (None, TFOOT command)
            | "body"                  -> (None, TBODY command)
            | "who"                   -> (Some Blk, BIB_AUTHOR command)
            | "what"                  -> (Some Blk, BIB_TITLE command)
            | "where"                 -> (Some Blk, BIB_RESOURCE command)
            | "arg"                   -> (None, MACROARG command)
            | x ->
                let maybe_assoc key xs = try Some (List.assoc key xs) with Not_found -> None in
                match maybe_assoc x tokenizer.inline_extdefs with
                    | Some Syn_empty        -> (Some Inl, INLPAT_EMPTY (command, x))
                    | Some Syn_seq          -> (Some Inl, INLPAT_SEQ (command, x))
                    | Some Syn_raw _        -> (Some Inl, INLPAT_RAW (command, x))
                    | Some Syn_raw_raw _    -> (Some Inl, INLPAT_RAW_RAW (command, x))
                    | Some Syn_raw_seq _    -> (Some Inl, INLPAT_RAW_SEQ (command, x))
                    | Some Syn_raw_seqopt _ -> (Some Inl, INLPAT_RAW_SEQOPT (command, x))
                    | Some _                -> assert false
                    | None -> match maybe_assoc x tokenizer.sim_block_extdefs with
                        | Some Syn_empty     -> (Some Blk, BLKPAT_EMPTY (command, x))
                        | Some Syn_seq       -> (Some Blk, BLKPAT_SEQ (command, x))
                        | Some Syn_raw _     -> (Some Blk, BLKPAT_RAW (command, x))
                        | Some Syn_raw_raw _ -> (Some Blk, BLKPAT_RAW_RAW (command, x))
                        | _                  -> (Some Inl, MACROCALL (command, x))


    let environment_command tokenizer raw_comm =
        let groups = Re.exec rex_env raw_comm in
        let primary = Re.get groups 4 in
        let command = build_command tokenizer primary groups 1 in
        match primary with
            | "abstract"                             -> (BEGIN_ABSTRACT command, END_ABSTRACT)
            | "itemize" | "itemise" | "ul"           -> (BEGIN_ITEMIZE command, END_ITEMIZE)
            | "enumerate" | "ol"                     -> (BEGIN_ENUMERATE command, END_ENUMERATE)
            | "description" | "dl"                   -> (BEGIN_DESCRIPTION command, END_DESCRIPTION)
            | "qanda"                                -> (BEGIN_QANDA command, END_QANDA)
            | "verse"                                -> (BEGIN_VERSE command, END_VERSE)
            | "quote"                                -> (BEGIN_QUOTE command, END_QUOTE)
            | "tabular"                              -> (BEGIN_TABULAR command, END_TABULAR)
            | "subpage"                              -> (BEGIN_SUBPAGE command, END_SUBPAGE)
            | "pull"                                 -> (BEGIN_PULLQUOTE command, END_PULLQUOTE)
            | "equation"                             -> (BEGIN_EQUATION command, END_EQUATION)
            | "printout"                             -> (BEGIN_PRINTOUT command, END_PRINTOUT)
            | "table"                                -> (BEGIN_TABLE command, END_TABLE)
            | "figure"                               -> (BEGIN_FIGURE command, END_FIGURE)
            | "bib"                                  -> (BEGIN_BIB command, END_BIB)
            | "note"                                 -> (BEGIN_NOTE command, END_NOTE)
            | x when String.starts_with x "mathtex"  -> tokenizer.literal <- Some x; (BEGIN_MATHTEX_BLK command, END_MATHTEX_BLK)
            | x when String.starts_with x "mathml"   -> tokenizer.literal <- Some x; (BEGIN_MATHML_BLK command, END_MATHML_BLK)
            | x when String.starts_with x "verbatim" -> tokenizer.literal <- Some x; (BEGIN_VERBATIM command, END_VERBATIM)
            | x when String.starts_with x "pre"      -> tokenizer.literal <- Some x; (BEGIN_VERBATIM command, END_VERBATIM)
            | x when String.starts_with x "source"   -> tokenizer.literal <- Some x; (BEGIN_SOURCE command, END_SOURCE)
            | x ->
                try
                    let (tag, syntax) = List.find (fun (tag, _) -> String.starts_with x tag) tokenizer.env_block_extdefs in
                    match syntax with
                        | Syn_lit  -> tokenizer.literal <- Some x; (BEGIN_BLKPAT_LIT (command, tag), END_BLKPAT_LIT)
                        | Syn_frag -> (BEGIN_BLKPAT_FRAG (command, tag), END_BLKPAT_FRAG)
                        | _        -> assert false
                with
                    Not_found -> (BEGIN_CUSTOM (command, primary), END_CUSTOM)

    let begin_command tokenizer raw_comm =
        fst (environment_command tokenizer raw_comm)

    let end_command tokenizer raw_comm =
        snd (environment_command tokenizer raw_comm)

    let cell_mark tokenizer raw_comm =
        let groups = Re.exec rex_cell_mark raw_comm in
        let get_group num =
            if Re.test groups num
            then Some (Re.get groups num)
            else None in
        let comm =
            {
            comm_tag = None;
            comm_label = get_group 1;
            comm_order = get_group 2;
            comm_style = get_group 3;
            comm_linenum = tokenizer.position.pos_lnum;
            comm_originator = Attr.Source;
            }
        in CELL_MARK comm

    let token_of_lexeme tokenizer = function
        | Begin_env raw_comm -> (Some Blk, begin_command tokenizer raw_comm)
        | End_env raw_comm   -> (Some Blk, end_command tokenizer raw_comm)
        | Begin_mathtex_inl  -> (Some Inl, BEGIN_MATHTEX_INL (build_op tokenizer))
        | End_mathtex_inl    -> (None, END_MATHTEX_INL)
        | Begin_mathml_inl   -> (Some Inl, BEGIN_MATHML_INL (build_op tokenizer))
        | End_mathml_inl     -> (None, END_MATHML_INL)
        | Simple raw_comm    -> simple_command tokenizer raw_comm
        | Text txt           -> (Some Inl, TEXT (build_op tokenizer, txt))
        | Entity ent         -> (Some Inl, ENTITY (build_op tokenizer, ent))
        | Cell_mark raw_comm -> (Some Blk, cell_mark tokenizer raw_comm)
        | Row_end            -> (Some Blk, ROW_END (build_op tokenizer))
        | Open               -> (None, OPEN)
        | Close              -> (None, CLOSE)
        | Eop                -> (Some Blk, END_INLINE)
        | Eof                -> (Some Blk, EOF)

    let next_lexing_triple tokenizer = match tokenizer.waiting_lexeme with
        | Some lexing_triple ->
            tokenizer.waiting_lexeme <- None;
            lexing_triple
        | None ->
            let lexer = match C.get () with
                | C.Block       -> Lexer.block
                | C.Inline      -> Lexer.inline
                | C.Raw         -> Lexer.raw
                | C.Mathtex_inl -> Lexer.mathtex_inl
                | C.Mathml_inl  -> Lexer.mathml_inl
                | C.Literal     -> match tokenizer.literal with
                    | Some l -> Lexer.literal l
                    | None   -> assert false in
            let outcome = lexer tokenizer.buffer in
            match outcome.previous with
                | Some previous ->
                    tokenizer.waiting_lexeme <- Some outcome.current;
                    previous
                | None ->
                    outcome.current

    let make ~linenum_offset ~inline_extdefs ~block_extdefs ~lexbuf =
        let (env_block_extdefs, sim_block_extdefs) = List.partition is_env block_extdefs in
        let position =
            {
            pos_fname = "";
            pos_lnum = linenum_offset + 1;
            pos_bol = 0;
            pos_cnum = 0;
            }
        in  {
            inline_extdefs;
            env_block_extdefs;
            sim_block_extdefs;
            buffer = Lexer.make_buffer lexbuf;
            position;
            waiting_lexeme = None;
            waiting_token = None;
            literal = None;
            }

    let rec next_token tokenizer = match tokenizer.waiting_token with
        | Some token ->
            tokenizer.waiting_token <- None;
            token
        | None ->
            let (lexeme, before, during) = next_lexing_triple tokenizer in
            tokenizer.position <- {tokenizer.position with pos_lnum = tokenizer.position.pos_lnum + before};
            let (trigger, token) = token_of_lexeme tokenizer lexeme in
            tokenizer.position <- {tokenizer.position with pos_lnum = tokenizer.position.pos_lnum + during};
            match (C.get (), trigger, token) with
                | (C.Block, Some Inl, _) ->
                    tokenizer.waiting_token <- Some token;
                    BEGIN_INLINE
                | (C.Inline, Some Blk, END_INLINE) ->
                    END_INLINE
                | (C.Inline, Some Blk, _) ->
                    tokenizer.waiting_token <- Some token;
                    END_INLINE
                | (C.Block, _, END_INLINE) ->
                    next_token tokenizer
                | _ ->
                    token

    let get_position tokenizer =
        tokenizer.position
end

