(********************************************************************************)
(*  Lambdoc_rlamblite_tokenizer.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module Parser = Lambdoc_rlamblite_parser
module Lexer = Lambdoc_rlamblite_lexer

open Lambdoc_prelude
open Lambdoc_core
open Lambdoc_reader
open Ast
open Lexing
open Lexer
open Parser


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

type limbo =
    | Empty
    | Numbered of string * Lexer.inline list
    | Unnumbered of Lexer.inline list list

type stack =
    | Nil
    | Ghost of Lexer.ghost
    | Uli of int * stack
    | Oli of int * stack

type tokenizer =
    {
    syntax: Lexer.syntax;
    lexbuf: Sedlexing.lexbuf;
    queue: Parser.token Queue.t;
    mutable environ: Lexer.inline list;
    mutable stack: stack;
    mutable limbo: limbo;
    mutable qprefix: string;
    mutable qlevel: int;
    mutable linenum: int;
    }


(********************************************************************************)
(** {1 Private functions and values}                                            *)
(********************************************************************************)

let count_char chr str =
    let total = ref 0 in
    let last = String.length str - 1 in
    for i = 0 to last do
        if str.[i] = chr then total := !total + 1
    done;
    !total

let base_comm =
    {
    comm_tag = None;
    comm_label = None;
    comm_order = None;
    comm_style = None;
    comm_linenum = 0;
    comm_originator = Attr.Source;
    }

let token_of_inline tokenizer comm inline =
    let check_environ mark begin_cons end_cons = match tokenizer.environ with
        | hd :: tl when hd = mark -> tokenizer.environ <- tl; end_cons
        | xs                      -> tokenizer.environ <- mark :: xs; begin_cons in
    match inline with
        | Plain txt  -> PLAIN (comm, txt)
        | Entity ent -> ENTITY (comm, ent)
        | Cite refs  -> CITE (comm, refs)
        | See refs   -> SEE (comm, refs)
        | Bold_mark  -> check_environ Bold_mark (BEGIN_BOLD comm) END_BOLD
        | Emph_mark  -> check_environ Emph_mark (BEGIN_EMPH comm) END_EMPH
        | Sup_mark   -> check_environ Sup_mark (BEGIN_SUP comm) END_SUP
        | Sub_mark   -> check_environ Sub_mark (BEGIN_SUB comm) END_SUB
        | Ins_mark   -> check_environ Ins_mark (BEGIN_INS comm) END_INS
        | Del_mark   -> check_environ Del_mark (BEGIN_DEL comm) END_DEL
        | Begin_caps -> BEGIN_CAPS comm
        | End_caps   -> END_CAPS
        | Begin_mono -> BEGIN_MONO comm
        | End_mono   -> END_MONO
        | Begin_link -> BEGIN_LINK comm
        | End_link   -> END_LINK
        | Link_sep   -> LINK_SEP

let get_nlevels str =
    1 + count_char '.' (String.rstrip ~chars:"." str)

let dump_seq comm tokenizer seq =
    List.iter (fun token -> Queue.push token tokenizer.queue) (List.map (token_of_inline tokenizer comm) seq)

let dump_seqs comm tokenizer seqs =
    let rec aux = function
        | hd1 :: hd2 :: tl ->
            dump_seq comm tokenizer hd1;
            Queue.push (Parser.PLAIN (comm, " ")) tokenizer.queue;
            aux (hd2 :: tl)
        | [seq] ->
            dump_seq comm tokenizer seq
        | [] ->
            () in
    aux (List.rev seqs)

let dump_limbo comm tokenizer = match tokenizer.limbo with
    | Empty ->
        ()
    | Numbered (number, _) ->
        raise Misplaced_numbered
    | Unnumbered seqs ->
        Queue.push (Parser.BEGIN_PAR comm) tokenizer.queue;
        dump_seqs comm tokenizer seqs;
        Queue.push Parser.END_PAR tokenizer.queue;
        tokenizer.limbo <- Empty

let rec dump_stack ?(limit = 0) tokenizer = match tokenizer.stack with
    | Ghost Sbib ->
        Queue.push Parser.END_SBIB tokenizer.queue;
        tokenizer.stack <- Nil
    | Ghost Note ->
        Queue.push Parser.END_NOTE tokenizer.queue;
        tokenizer.stack <- Nil
    | Uli (x, s) when x > limit ->
        Queue.push Parser.END_ITEMIZE tokenizer.queue;
        tokenizer.stack <- s;
        dump_stack ~limit tokenizer
    | Oli (x, s) when x > limit ->
        Queue.push Parser.END_ENUMERATE tokenizer.queue;
        tokenizer.stack <- s;
        dump_stack ~limit tokenizer
    | Nil | Uli _ | Oli _ ->
        ()

let align_stack ilevel tokenizer = match tokenizer.stack with
    | Nil when ilevel > 0 ->
        raise Bad_indentation
    | (Uli (x, _) | Oli (x, _)) when ilevel <= x ->
       dump_stack ~limit:ilevel tokenizer
    | _ ->
        ()

let close_quote tokenizer =
    if tokenizer.qlevel > 0
    then for i = 1 to tokenizer.qlevel do Queue.push Parser.END_QUOTE tokenizer.queue done

let issue_section comm order seq rule tokenizer =
    let comm' = {comm with comm_order = order} in
    let slevel = match rule with
        | Double -> 1
        | Single -> 2 in
    Queue.push (Parser.BEGIN_SECTION (comm', slevel)) tokenizer.queue;
    dump_seq comm tokenizer seq;
    Queue.push Parser.END_SECTION tokenizer.queue;
    tokenizer.limbo <- Empty

let rec produce tokenizer =
    let lexeme = Lexer.next ~syntax:tokenizer.syntax tokenizer.lexbuf in
    let comm = {base_comm with comm_linenum = tokenizer.linenum} in
    match lexeme with
        | Eof ->
            dump_limbo comm tokenizer;
            dump_stack tokenizer;
            close_quote tokenizer;
            Queue.push Parser.EOF tokenizer.queue
        | Regular (qprefix, iprefix, regular) ->
            let qlevel = count_char '>' qprefix in
            (if (qlevel = tokenizer.qlevel && qprefix <> tokenizer.qprefix) then raise (Mismatched_quotation (tokenizer.qprefix, qprefix)));
            let ilevel = String.length iprefix in
            let qdiff = qlevel - tokenizer.qlevel in
            if qdiff <> 0
            then begin
                dump_limbo comm tokenizer;
                dump_stack tokenizer;
                tokenizer.qprefix <- qprefix;
                tokenizer.qlevel <- qlevel;
                let token = if qdiff > 0 then Parser.BEGIN_QUOTE comm else Parser.END_QUOTE in
                for i = 1 to abs qdiff do Queue.push token tokenizer.queue done
            end;
            begin match regular with
                | Rule rule ->
                    begin match tokenizer.limbo with
                        | Empty ->
                            Queue.push (Parser.RULE comm) tokenizer.queue
                        | Numbered (number, seq) ->
                            begin match (get_nlevels number, rule) with
                                | (2, Single)
                                | (1, Double) -> ()
                                | (n, rule)   -> raise (Mismatched_rule (n, rule))
                            end;
                            issue_section comm None seq rule tokenizer
                        | Unnumbered [seq] ->
                            issue_section comm (Some "") seq rule tokenizer
                        | Unnumbered _ ->
                            dump_limbo comm tokenizer;
                            Queue.push (Parser.RULE comm) tokenizer.queue
                    end
                | Literal (literal, nlines, style, raw) ->
                    dump_limbo comm tokenizer;
                    align_stack ilevel tokenizer;
                    let comm = match style with "" -> comm | _ -> {comm with comm_style = Some style} in
                    let token = match literal with
                        | Source   -> Parser.SOURCE (comm, raw)
                        | Verbatim -> Parser.VERBATIM (comm, raw) in
                    Queue.push token tokenizer.queue;
                    tokenizer.linenum <- tokenizer.linenum + nlines + 1
                | Section (sprefix, seq) ->
                    dump_limbo comm tokenizer;
                    dump_stack tokenizer;
                    let slevel = count_char '=' sprefix in
                    Queue.push (Parser.BEGIN_SECTION (comm, slevel)) tokenizer.queue;
                    dump_seq comm tokenizer seq;
                    Queue.push Parser.END_SECTION tokenizer.queue
                | Ghost (ghost, label, seq) ->
                    assert (qprefix = "" && iprefix = "");
                    let lprefix = match ghost with
                        | Sbib -> "bib:"
                        | Note -> "note:" in
                    dump_limbo comm tokenizer;
                    dump_stack tokenizer;
                    let comm = {comm with comm_label = Some (lprefix ^ label)} in
                    let token = match ghost with
                        | Sbib -> Parser.BEGIN_SBIB comm
                        | Note -> Parser.BEGIN_NOTE comm in
                    tokenizer.limbo <- Unnumbered [seq];
                    tokenizer.stack <- Ghost ghost;
                    Queue.push token tokenizer.queue;
                | Textual (None, []) ->
                    dump_limbo comm tokenizer
                | Textual (None, seq) ->
                    align_stack ilevel tokenizer;
                    tokenizer.limbo <-
                        begin match tokenizer.limbo with
                            | Empty         -> Unnumbered [seq]
                            | Unnumbered xs -> Unnumbered (seq :: xs)
                            | Numbered _    -> assert false
                        end
                | Textual (Some tprefix, []) ->
                    raise (Empty_tprefix tprefix)
                | Textual (Some Sec number, seq) when ilevel <> 0 ->
                    raise Misaligned_section
                | Textual (Some Sec number, seq) ->
                    dump_limbo comm tokenizer;
                    tokenizer.limbo <- Numbered (number, seq)
                | Textual (Some Oli number, seq) when ilevel = 0 ->
                    dump_limbo comm tokenizer;
                    tokenizer.limbo <- Numbered (number, seq)
                | Textual (Some Oli _, seq) ->
                    dump_limbo comm tokenizer;
                    tokenizer.limbo <- Unnumbered [seq];
                    begin match tokenizer.stack with
                        | Oli (x, stack) when ilevel = x ->
                            ()
                        | (Oli (x, stack) | Uli (x, stack)) when ilevel < x ->
                            dump_stack ~limit:ilevel tokenizer
                        | Uli (x, stack) when ilevel = x ->
                            Queue.push Parser.END_ITEMIZE tokenizer.queue;
                            tokenizer.stack <- Oli (ilevel, stack);
                            Queue.push (Parser.BEGIN_ENUMERATE comm) tokenizer.queue;
                        | stack ->
                            tokenizer.stack <- Oli (ilevel, stack);
                            Queue.push (Parser.BEGIN_ENUMERATE comm) tokenizer.queue;
                    end;
                    Queue.push (Parser.ITEM comm) tokenizer.queue
                | Textual (Some Uli, seq) when ilevel = 0 ->
                    raise Misaligned_listing
                | Textual (Some Uli, seq) ->
                    dump_limbo comm tokenizer;
                    tokenizer.limbo <- Unnumbered [seq];
                    begin match tokenizer.stack with
                        | Uli (x, stack) when ilevel = x ->
                            ()
                        | (Uli (x, stack) | Oli (x, stack)) when ilevel < x ->
                            dump_stack ~limit:ilevel tokenizer
                        | Oli (x, stack) when ilevel = x ->
                            Queue.push Parser.END_ENUMERATE tokenizer.queue;
                            tokenizer.stack <- Uli (ilevel, stack);
                            Queue.push (Parser.BEGIN_ITEMIZE comm) tokenizer.queue;
                        | stack ->
                            tokenizer.stack <- Uli (ilevel, stack);
                            Queue.push (Parser.BEGIN_ITEMIZE comm) tokenizer.queue;
                    end;
                    Queue.push (Parser.ITEM comm) tokenizer.queue
            end;
            tokenizer.linenum <- tokenizer.linenum + 1


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let make ?(options = `Lambwiki) ~linenum_offset str =
    {
    syntax = options;
    lexbuf = Sedlexing.Utf8.from_string str;
    queue = Queue.create ();
    environ = [];
    stack = Nil;
    limbo = Empty;
    qlevel = 0;
    qprefix = "";
    linenum = linenum_offset + 1;
    }

let rec next_token tokenizer =
    if Queue.is_empty tokenizer.queue
    then (produce tokenizer; next_token tokenizer)
    else Queue.pop tokenizer.queue

let get_position tokenizer =
    {
    pos_fname = "";
    pos_lnum = tokenizer.linenum;
    pos_bol = 0;
    pos_cnum = 0;
    }

