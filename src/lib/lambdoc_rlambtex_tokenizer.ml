(********************************************************************************)
(*  Lambdoc_rlambtex_tokenizer.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
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
        lexbuf: Sedlexing.lexbuf;
        mutable position: Lexing.position;
        mutable waiting: Lexer.pair option;
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
            | "br"                    -> LINEBREAK command
            | "glyph"                 -> GLYPH command
            | "bold" | "strong" | "b" -> BOLD command
            | "emph" | "em" | "i"     -> EMPH command
            | "code" | "tt"           -> CODE command
            | "caps"                  -> CAPS command
            | "ins"                   -> INS command
            | "del"                   -> DEL command
            | "sup"                   -> SUP command
            | "sub"                   -> SUB command
            | "mbox"                  -> MBOX command
            | "span"                  -> SPAN command
            | "link" | "a"            -> LINK command
            | "see"                   -> SEE command
            | "cite"                  -> CITE command
            | "dref"                  -> DREF command
            | "sref"                  -> SREF command
            | "mref"                  -> MREF command
            | "paragraph" | "p"       -> PARAGRAPH command
            | "picture"               -> PICTURE command
            | "part"                  -> PART command
            | "appendix"              -> APPENDIX command
            | "h1" | "section"        -> SECTION (command, 1)
            | "h2" | "subsection"     -> SECTION (command, 2)
            | "h3" | "subsubsection"  -> SECTION (command, 3)
            | "h4"                    -> SECTION (command, 4)
            | "h5"                    -> SECTION (command, 5)
            | "h6"                    -> SECTION (command, 6)
            | "bibliography"          -> BIBLIOGRAPHY command
            | "notes"                 -> NOTES command
            | "toc"                   -> TOC command
            | "title"                 -> TITLE (command, 1)
            | "subtitle"              -> TITLE (command, 2)
            | "rule" | "hr"           -> RULE command
            | "newmacro"              -> MACRODEF command
            | "newboxout"             -> BOXOUTDEF command
            | "newtheorem"            -> THEOREMDEF command
            | "item" | "li"           -> ITEM command
            | "question"              -> QUESTION command
            | "rquestion"             -> RQUESTION command
            | "answer"                -> ANSWER command
            | "ranswer"               -> RANSWER command
            | "head"                  -> THEAD command
            | "foot"                  -> TFOOT command
            | "body"                  -> TBODY command
            | "who"                   -> BIB_AUTHOR command
            | "what"                  -> BIB_TITLE command
            | "where"                 -> BIB_RESOURCE command
            | "arg"                   -> MACROARG command
            | x ->
                let maybe_assoc key xs = try Some (List.assoc key xs) with Not_found -> None in
                match maybe_assoc x tokenizer.inline_extdefs with
                    | Some Syn_empty        -> INLPAT_EMPTY (command, x)
                    | Some Syn_seq          -> INLPAT_SEQ (command, x)
                    | Some Syn_raw _        -> INLPAT_RAW (command, x)
                    | Some Syn_raw_raw _    -> INLPAT_RAW_RAW (command, x)
                    | Some Syn_raw_seq _    -> INLPAT_RAW_SEQ (command, x)
                    | Some Syn_raw_seqopt _ -> INLPAT_RAW_SEQOPT (command, x)
                    | Some _                -> assert false
                    | None -> match maybe_assoc x tokenizer.sim_block_extdefs with
                        | Some Syn_empty     -> BLKPAT_EMPTY (command, x)
                        | Some Syn_seq       -> BLKPAT_SEQ (command, x)
                        | Some Syn_raw _     -> BLKPAT_RAW (command, x)
                        | Some Syn_raw_raw _ -> BLKPAT_RAW_RAW (command, x)
                        | _                  -> MACROCALL (command, x)


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
            | x when String.starts_with x "mathtex"  -> tokenizer.literal <- Some x; (BEGIN_MATHTEXBLK command, END_MATHTEXBLK)
            | x when String.starts_with x "mathml"   -> tokenizer.literal <- Some x; (BEGIN_MATHMLBLK command, END_MATHMLBLK)
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
        | Begin_env raw_comm -> begin_command tokenizer raw_comm
        | End_env raw_comm   -> end_command tokenizer raw_comm
        | Begin_mathtexinl   -> BEGIN_MATHTEXINL (build_op tokenizer)
        | End_mathtexinl     -> END_MATHTEXINL (build_op tokenizer)
        | Begin_mathmlinl    -> BEGIN_MATHMLINL (build_op tokenizer)
        | End_mathmlinl      -> END_MATHMLINL (build_op tokenizer)
        | Open               -> OPEN
        | Close              -> CLOSE
        | Simple raw_comm    -> simple_command tokenizer raw_comm
        | Cell_mark raw_comm -> cell_mark tokenizer raw_comm
        | Row_end            -> ROW_END (build_op tokenizer)
        | Eof                -> EOF
        | Par_break          -> PAR_BREAK (build_op tokenizer)
        | Space              -> SPACE (build_op tokenizer)
        | Text txt           -> TEXT (build_op tokenizer, txt)
        | Entity ent         -> ENTITY (build_op tokenizer, ent)

    let next_lexing_pair tokenizer = match tokenizer.waiting with
        | Some lexing_pair ->
            tokenizer.waiting <- None;
            lexing_pair
        | None ->
            let lexer = match C.get () with
                | C.General    -> Lexer.general
                | C.Raw        -> Lexer.raw
                | C.Mathtexinl -> Lexer.mathtexinl
                | C.Mathmlinl  -> Lexer.mathmlinl
                | C.Literal    -> match tokenizer.literal with
                    | Some l -> Lexer.literal l
                    | None   -> assert false in
            let outcome = lexer tokenizer.lexbuf in
            match outcome.previous with
                | Some previous ->
                    tokenizer.waiting <- Some outcome.current;
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
            lexbuf;
            position;
            waiting = None;
            literal = None;
            }

    let next_token tokenizer =
        let (lexeme, nlines) = next_lexing_pair tokenizer in
        let token = token_of_lexeme tokenizer lexeme in
        tokenizer.position <- {tokenizer.position with pos_lnum = tokenizer.position.pos_lnum + nlines};
        token

    let get_position tokenizer =
        tokenizer.position
end

