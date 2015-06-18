(********************************************************************************)
(*  Lambdoc_rlambwiki_tokenizer.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Tokenizer for the Lambwiki reader.
*)

module String = BatString
module Parser = Lambdoc_rlambwiki_parser
module Scanner = Lambdoc_rlambwiki_scanner

open Lexing
open Scanner
open Parser


(********************************************************************************)
(*  {1 Exceptions}                                                              *)
(********************************************************************************)

exception Invalid_ulist_level of int * int
exception Invalid_olist_level of int * int


(********************************************************************************)
(*  {1 Type definitions}                                                        *)
(********************************************************************************)

type context_t =
    | General
    | Source
    | Verbatim


(********************************************************************************)
(*  {1 Tokenizer class}                                                         *)
(********************************************************************************)

class tokenizer =
let rex = Pcre.regexp "\\r\\n|\\n" in   (* We expect lines to be terminated with '\r\n' or just '\n' *)
fun ~linenum_offset str ->
let lines = Pcre.asplit ~rex ~max:(-1) str in
object (self)

    val lines = lines
    val mutable line_counter = 0
    val mutable quote_state = 0
    val mutable list_state = []
    val mutable par_state = false
    val mutable productions = []
    val mutable context = General


    method private op =
        let open Lambdoc_reader.Ast in
        {
        comm_tag = None;
        comm_label = None;
        comm_order = None;
        comm_style = None;
        comm_linenum = linenum_offset + line_counter + 1;
        }


    method private comm ?(tag = None) ?(label = None) ?(order = None) ?(style = None) () =
        let open Lambdoc_reader.Ast in
        {
        comm_tag = tag;
        comm_label = label;
        comm_order = order;
        comm_style = style;
        comm_linenum = linenum_offset + line_counter + 1;
        }


    method private store token =
        productions <- match (productions, token) with
            | ([PLAIN (op, txt1)], PLAIN (_, txt2)) -> [PLAIN (op, txt1 ^ txt2)]
            | ([RAW txt1], RAW txt2)                -> [RAW (txt1 ^ "\n" ^ txt2)]
            | _                                     -> productions @ [token]


    method private tokens_of_text text_list =
        let conv = function
            | Plain txt  -> PLAIN (self#op, txt)
            | Entity ent -> ENTITY (self#op, ent)
            | Bold_mark  -> BOLD_MARK self#op
            | Emph_mark  -> EMPH_MARK self#op
            | Sup_mark   -> SUP_MARK self#op
            | Sub_mark   -> SUB_MARK self#op
            | Begin_caps -> BEGIN_CAPS self#op
            | End_caps   -> END_CAPS self#op
            | Begin_code -> BEGIN_CODE self#op
            | End_code   -> END_CODE self#op
            | Begin_link -> BEGIN_LINK self#op
            | End_link   -> END_LINK self#op
            | Link_sep   -> LINK_SEP self#op
        in List.map conv text_list


    method private token_of_list = function
        | Ulist, true  -> BEGIN_ITEMIZE self#op
        | Ulist, false -> END_ITEMIZE self#op
        | Olist, true  -> BEGIN_ENUMERATE self#op
        | Olist, false -> END_ENUMERATE self#op


    method private unwind_list new_level =
        let rec trim_list c = match (c, list_state) with
            | x, kind :: tl when x > 0 ->
                begin
                    self#store (self#token_of_list (kind, false));
                    list_state <- tl;
                    trim_list (c - 1)
                end
            | _ ->
                ()
        in trim_list ((List.length list_state) - new_level)


    method private unset_par =
        if par_state
        then self#store (END_PAR self#op); par_state <- false


    method ajust_quote target =
        let rec aux = function
            | x when x > 0  -> self#store (BEGIN_QUOTE self#op); aux (x - 1)
            | x when x < 0  -> self#store (END_QUOTE self#op); aux (x + 1)
            | _     -> ()
        in aux (target - quote_state)


    method private handle_par quote_new list_new text =

        if quote_new <> quote_state
        then begin
            self#unset_par;
            self#unwind_list 0;
            self#ajust_quote quote_new;
            quote_state <- quote_new
        end;

        let () = match (list_state, list_new) with

            | x, Some (kind, level) when level - (List.length x) = 1 ->
                begin
                    self#unset_par;
                    self#store (self#token_of_list (kind, true));
                    self#store (ITEM self#op);
                    list_state <- kind :: list_state
                end

            | x, Some (kind, level) when level <= List.length x ->
                begin
                    self#unset_par;
                    self#unwind_list level;
                    match list_state with
                        | top::tl when top = kind ->
                            self#store (ITEM self#op);
                        | _ ->
                            begin
                                self#unwind_list (level - 1);
                                self#store (self#token_of_list (kind, true));
                                self#store (ITEM self#op);
                                list_state <- kind :: list_state
                            end
                end

            | x, Some (kind, level) ->
                begin
                    match kind with
                        | Ulist -> raise (Invalid_ulist_level ((List.length x), level))
                        | Olist -> raise (Invalid_olist_level ((List.length x), level))
                end

            | hd::tl, None when not par_state && (List.length text) > 0 ->
                begin
                    self#unwind_list 0
                end
                
            | _ ->
                begin
                    ()
                end in

        let tokens = self#tokens_of_text text in
        let () = match (par_state, tokens) with
            | false, hd::tl -> List.iter self#store ((BEGIN_PAR self#op) :: tokens)
            | true, hd::tl  -> List.iter self#store (PLAIN (self#op, " ") :: tokens)
            | true, []      -> self#store (END_PAR self#op)
            | _             -> ()
        in par_state <- text <> [] 


    method private produce =
        if line_counter >= Array.length lines
        then begin
            self#unset_par;
            self#unwind_list 0;
            productions <- productions @ [EOF]
        end
        else begin
            let scanner = match context with
                | General  -> Scanner.general_scanner
                | Source   -> Scanner.source_scanner
                | Verbatim -> Scanner.verbatim_scanner in
            let lexbuf = Ulexing.from_utf8_string lines.(line_counter) in
            let tok = scanner lexbuf in
            let () = match tok with
                | Begin_source style ->
                    self#unwind_list 0;
                    context <- Source;
                    let style = if String.length style = 0 then None else Some style in
                    self#store (BEGIN_SOURCE (self#comm ~tag:(Some "{{{") ~style ()))
                | End_source ->
                    context <- General;
                    self#store (END_SOURCE self#op)
                | Begin_verbatim style ->
                    self#unwind_list 0;
                    context <- Verbatim;
                    let style = if String.length style = 0 then None else Some style in
                    self#store (BEGIN_VERBATIM (self#comm ~tag:(Some "(((") ~style ()))
                | End_verbatim ->
                    context <- General;
                    self#store (END_VERBATIM self#op)
                | Section (level, text) ->
                    self#unwind_list 0;
                    self#store (BEGIN_SECTION (self#op, level));
                    List.iter self#store (self#tokens_of_text text);
                    self#store (END_SECTION self#op)
                | Par (quote_new, list_new, text) ->
                    self#handle_par quote_new list_new text
                | Raw txt ->
                    self#store (RAW txt)
            in line_counter <- line_counter + 1
        end

    method consume = match productions with
        | []
        | [PLAIN (_, _)]
        | [RAW _]  -> self#produce; self#consume
        | hd :: tl -> productions <- tl; hd

    method position =
        {
        pos_fname = "";
        pos_lnum = linenum_offset + line_counter + 1;
        pos_bol = 0;
        pos_cnum = 0;
        }
end

