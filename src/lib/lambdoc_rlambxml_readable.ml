(********************************************************************************)
(*  Lambdoc_rlambxml_readable.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_reader
open Extension
open Ast

module Hashtbl = BatHashtbl
module List = BatList
module String = BatString


(********************************************************************************)
(** {1 Private modules}                                                         *)
(********************************************************************************)

module Stringset = Set.Make (String)

module Errors =
struct
    type msg =
        | Unknown_attribute of string
        | Missing_attribute of string
        | Unknown_tag
        | Expected_empty
        | Inline_in_block
        | Bad_dl
        | Expected_seq_and_frag
        | Raw_text

    type error = Ast.command * msg

    type t = error list ref

    let make () =
        ref []

    let add errors comm msg =
        errors := (comm, msg) :: !errors

    let to_reading errors =
        let f (comm, msg) =
            let desc = match msg with
                | Unknown_attribute a   -> Printf.sprintf "Attribute '#%s#' does not belong to this element" a
                | Missing_attribute a   -> Printf.sprintf "Mandatory attribute '#%s#' has not been provided" a
                | Unknown_tag           -> "Element is unknown"
                | Expected_empty        -> "Element should be empty"
                | Inline_in_block       -> "Inline content where block expected"
                | Bad_dl                -> "Bad description list"
                | Expected_seq_and_frag -> "Expected seq and frag"
                | Raw_text              -> "Raw text"
            in (Some comm.comm_linenum, comm.comm_tag, desc)
        in List.rev_map f !errors
end

module Prop =
struct
    type t =
        {
        attrs: Xmlm.attribute list;
        dict: (string, string) Hashtbl.t;
        }

    type _ param =
        | Req: string -> string param
        | Opt: string -> string option param
        | List: string -> string list param

    let to_string prop =
        List.map (fun ((_, k), v) -> Printf.sprintf " %s=\"%s\"" k v) prop.attrs |> String.join ""

    let make attrs =
        let prop = {attrs; dict = Hashtbl.create 10} in
        let f ((_, k), v) = Hashtbl.add prop.dict k v in
        List.iter f attrs;
        prop

    let get: type p. t -> p param -> p = fun prop param ->
        let fetch k =
            let v = Hashtbl.find prop.dict k in
            Hashtbl.remove prop.dict k;
            v in
        match param with
            | Req k  -> fetch k
            | Opt k  -> (try Some (fetch k) with Not_found -> None)
            | List k -> (try String.nsplit (fetch k) " " with Not_found -> [])

    let all prop =
        Hashtbl.fold (fun k _ accum -> k :: accum) prop.dict []
end


(********************************************************************************)
(** {1 Private exceptions}                                                      *)
(********************************************************************************)

exception Inline_in_block


(********************************************************************************)
(** {1 Private type definitions}                                                *)
(********************************************************************************)

type tree = Ast.command * Prop.t * node
and node =
    | E of string * tree list
    | D of string


(********************************************************************************)
(** {1 Private functions and values}                                            *)
(********************************************************************************)

let cleanup ?empty comm prop errors =
    begin match empty with
        | None | Some [] -> ()
        | Some _         -> Errors.(add errors comm Expected_empty)
    end;
    List.iter (fun k -> Errors.(add errors comm (Unknown_attribute k))) (Prop.all prop)


let guarded_get: type p. Ast.command -> Prop.t -> Errors.t -> p Prop.param -> p = fun comm prop errors param ->
    match param with
        | Prop.Req a  -> (try Prop.get prop param with Not_found -> (Errors.(add errors comm (Missing_attribute a)); ""))
        | _           -> Prop.get prop param


let make0: ?empty:tree list -> Ast.command -> Prop.t -> Errors.t -> (unit -> 'r) -> Ast.command * 'r =
    fun ?empty comm prop errors f ->
        cleanup ?empty comm prop errors;
        (comm, f ())


let make1: type p1. ?empty:tree list -> Ast.command -> Prop.t -> Errors.t -> p1 Prop.param -> (p1 -> 'r) -> Ast.command * 'r =
    fun ?empty comm prop errors param1 f ->
        let a1 = guarded_get comm prop errors param1 in
        cleanup ?empty comm prop errors;
        (comm, f a1)


let make2: type p1 p2. ?empty:tree list -> Ast.command -> Prop.t -> Errors.t -> p1 Prop.param -> p2 Prop.param -> (p1 -> p2 -> 'r) -> Ast.command * 'r =
    fun ?empty comm prop errors param1 param2 f ->
        let a1 = guarded_get comm prop errors param1 in
        let a2 = guarded_get comm prop errors param2 in
        cleanup ?empty comm prop errors;
        (comm, f a1 a2)


let anonymous_comm (lnum, _) =
    {
    comm_tag = None;
    comm_label = None;
    comm_order = None;
    comm_style = None;
    comm_linenum = lnum;
    comm_originator = Source;
    }


let tagged_comm tag (lnum, _) prop =
    {
    comm_tag = Some tag;
    comm_label = Prop.get prop (Opt "label");
    comm_order = Prop.get prop (Opt "order");
    comm_style = Prop.get prop (Opt "style");
    comm_linenum = lnum;
    comm_originator = Source;
    }


let is_whitespace =
    let rex = Re.(compile (seq [bos; rep1 Re.space; eos])) in
    fun str -> Re.execp rex str


let rec tree_of_input i =
    let pos = Xmlm.pos i in
    match Xmlm.input i with
        | `Dtd _ ->
            tree_of_input i
        | `El_start tag ->
            let rec aux i headers context =
                let pos = Xmlm.pos i in
                match Xmlm.input i with
                    | `El_start tag -> aux i ((pos, tag) :: headers) ([] :: context)
                    | `El_end ->
                        begin match (headers, context) with
                            | (header :: headers', children :: context') ->
                                let (pos, ((_, local), attrs)) = header in
                                let prop = Prop.make attrs in
                                let comm = tagged_comm local pos prop in
                                let e = (comm, prop, E (local, List.rev children)) in
                                begin match context' with
                                    | parent :: context'' -> aux i headers' ((e :: parent) :: context'')
                                    | []                  -> e
                                end
                            | _ -> assert false
                        end
                    | `Data txt when is_whitespace txt ->
                        aux i headers context
                    | `Data txt ->
                        begin match context with
                            | children :: context' ->
                                let prop = Prop.make [] in
                                let comm = anonymous_comm pos in
                                let d = (comm, prop, D txt) in
                                aux i headers ((d :: children) :: context')
                            | [] -> assert false
                        end
                    | `Dtd _ -> assert false
            in aux i ((pos, tag) :: []) ([] :: [])
        | _ -> assert false


let deep_copy trees =
    let buf = Buffer.create 256 in
    let rec copy_list xs = List.iter copy_one xs
    and copy_one (comm, prop, node) = match node with
        | E (tag, []) ->
            Buffer.add_char buf '<';
            Buffer.add_string buf tag;
            Buffer.add_string buf (Prop.to_string prop);
            Buffer.add_string buf "/>"
        | E (tag, children) -> 
            Buffer.add_char buf '<';
            Buffer.add_string buf tag;
            Buffer.add_string buf (Prop.to_string prop);
            Buffer.add_char buf '>';
            copy_list children;
            Buffer.add_string buf "</";
            Buffer.add_string buf tag;
            Buffer.add_char buf '>'
        | D d ->
            Buffer.add_string buf d in
    copy_list trees;
    Buffer.contents buf


let dummy_inline = Ast.Linebreak


let dummy_block = Ast.Paragraph []


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let ast_from_string ~linenum_offset ~inline_extdefs ~block_extdefs str =

    let errors = Errors.make () in
    let newmacros = ref Stringset.empty in
    let newcustoms = ref Stringset.empty in

    let literal_of_trees = function
        | [(_, _, D txt)] -> txt
        | _               -> assert false in

    let rec seq_of_trees xs = List.map inline_of_tree xs

    and maybe_seq_of_trees = function
        | [] -> None
        | xs -> Some (seq_of_trees xs)

    and inline_of_tree (comm, prop, node) = match node with
        | D txt                             -> make0 comm prop errors (fun () -> Ast.Plain txt)
        | E ("br", xs)                      -> make0 ~empty:xs comm prop errors (fun () -> Ast.Linebreak)
        | E ("mathtexinl", xs)              -> make0 comm prop errors (fun () -> Ast.Mathtex_inl (literal_of_trees xs))
        | E ("mathmlinl", xs)               -> make0 comm prop errors (fun () -> Ast.Mathml_inl (deep_copy xs))
        | E ("glyph", xs)                   -> make2 ~empty:xs comm prop errors (Req "src") (Req "alt") (fun src alt -> Ast.Glyph (src, alt))
        | E (("bold" | "strong" | "b"), xs) -> make0 comm prop errors (fun () -> Ast.Bold (seq_of_trees xs))
        | E (("emph" | "em" | "i"), xs)     -> make0 comm prop errors (fun () -> Ast.Emph (seq_of_trees xs))
        | E (("mono" | "tt"), xs)           -> make0 comm prop errors (fun () -> Ast.Mono (seq_of_trees xs))
        | E ("caps", xs)                    -> make0 comm prop errors (fun () -> Ast.Caps (seq_of_trees xs))
        | E ("ins", xs)                     -> make0 comm prop errors (fun () -> Ast.Ins (seq_of_trees xs))
        | E ("del", xs)                     -> make0 comm prop errors (fun () -> Ast.Del (seq_of_trees xs))
        | E ("sup", xs)                     -> make0 comm prop errors (fun () -> Ast.Sup (seq_of_trees xs))
        | E ("sub", xs)                     -> make0 comm prop errors (fun () -> Ast.Sub (seq_of_trees xs))
        | E ("mbox", xs)                    -> make0 comm prop errors (fun () -> Ast.Mbox (seq_of_trees xs))
        | E ("span", xs)                    -> make0 comm prop errors (fun () -> Ast.Span (seq_of_trees xs))
        | E (("link" | "a"), xs)            -> make1 comm prop errors (Req "href") (fun href -> Ast.Link (href, maybe_seq_of_trees xs))
        | E ("see", xs)                     -> make1 ~empty:xs comm prop errors (List "href") (fun hrefs -> Ast.See hrefs)
        | E ("cite", xs)                    -> make1 ~empty:xs comm prop errors (List "href") (fun hrefs -> Ast.Cite hrefs)
        | E ("dref", xs)                    -> make1 comm prop errors (Req "href") (fun href -> Ast.Dref (href, maybe_seq_of_trees xs))
        | E ("sref", xs)                    -> make1 comm prop errors (Req "href") (fun href -> Ast.Sref (href, maybe_seq_of_trees xs))
        | E ("mref", xs)                    -> make1 comm prop errors (Req "href") (fun href -> Ast.Mref (href, seq_of_trees xs))
        | E ("arg", xs)                     -> make1 comm prop errors (Req "num") (fun num -> Ast.Macroarg num)
        | E (tag, xs) ->
            try
                let (comm, inlpat) = match List.assoc tag inline_extdefs with
                    | Syn_empty            -> make0 ~empty:xs comm prop errors (fun () -> Inlpat_empty)
                    | Syn_seq              -> make0 comm prop errors (fun () -> Inlpat_seq (seq_of_trees xs))
                    | Syn_raw a            -> make1 ~empty:xs comm prop errors (Req a) (fun a -> Inlpat_raw a)
                    | Syn_raw_raw (a1, a2) -> make2 comm prop errors (Req a1) (Req a2) (fun a1 a2 -> Inlpat_raw_raw (a1, a2))
                    | Syn_raw_seq a        -> make1 ~empty:xs comm prop errors (Req a) (fun a -> Inlpat_raw_seq (a, seq_of_trees xs))
                    | Syn_raw_seqopt a     -> make1 ~empty:xs comm prop errors (Req a) (fun a -> Inlpat_raw_seqopt (a, maybe_seq_of_trees xs))
                    | _ -> assert false
                in (comm, Extcomm_inl (tag, inlpat))
            with Not_found ->
                if Stringset.mem tag !newmacros
                then make0 comm prop errors (fun () -> Macrocall (tag, []))
                else (Errors.add errors comm Unknown_tag; (comm, dummy_inline)) in

    let rec frag_of_trees ?flow xs = List.map (block_of_tree ?flow) xs

    and block_of_tree ?(flow = false) (comm, prop, node) = match node with
        | D _                              -> if flow then raise Inline_in_block else (Errors.add errors comm Raw_text; (comm, dummy_block))
        | E (("paragraph" | "p"), xs)      -> make0 comm prop errors (fun () -> Paragraph (seq_of_trees xs))
        | E (("itemize" | "ul"), xs)       -> make0 comm prop errors (fun () -> Itemize (anon_frags_of_trees xs))
        | E (("enumerate" | "ol"), xs)     -> make0 comm prop errors (fun () -> Enumerate (anon_frags_of_trees xs))
        | E (("description" | "dl"), xs)   -> make0 comm prop errors (fun () -> Description (desc_frags_of_trees comm xs))
        | E ("qanda", xs)                  -> make0 comm prop errors (fun () -> Qanda (qanda_frags_of_trees xs))
        | E ("verse", xs)                  -> make0 comm prop errors (fun () -> Verse (frag_of_trees xs))
        | E ("quote", xs)                  -> make0 comm prop errors (fun () -> Quote (frag_of_trees xs))
        | E ("mathtexblk", xs)             -> make0 comm prop errors (fun () -> Mathtex_blk (literal_of_trees xs))
        | E ("mathmlblk", xs)              -> make0 comm prop errors (fun () -> Mathml_blk (deep_copy xs))
        | E ("source", xs)                 -> make0 comm prop errors (fun () -> Source (literal_of_trees xs))
        | E ("tabular", xs)                -> make0 comm prop errors (fun () -> Tabular (tabular_of_trees xs))
        | E ("subpage", xs)                -> make0 comm prop errors (fun () -> Subpage (frag_of_trees xs))
        | E (("verbatim" | "pre"), xs)     -> make0 comm prop errors (fun () -> Verbatim (literal_of_trees xs))
        | E ("picture", xs)                -> make2 ~empty:xs comm prop errors (Req "src") (Req "alt") (fun src alt -> Picture (src, alt))
        | E ("pull", xs)                   -> make0 comm prop errors (fun () -> let (s, f) = seq_and_frag_of_trees comm xs in Pullquote (s, f))
        | E ("equation", xs)               -> make0 comm prop errors (fun () -> let (s, b) = wrapper_of_trees xs in Equation (s, b))
        | E ("printout", xs)               -> make0 comm prop errors (fun () -> let (s, b) = wrapper_of_trees xs in Printout (s, b))
        | E ("table", xs)                  -> make0 comm prop errors (fun () -> let (s, b) = wrapper_of_trees xs in Table (s, b))
        | E ("figure", xs)                 -> make0 comm prop errors (fun () -> let (s, b) = wrapper_of_trees xs in Figure (s, b))
        | E ("part", xs)                   -> make0 comm prop errors (fun () -> Part (seq_of_trees xs))
        | E ("appendix", xs)               -> make0 ~empty:xs comm prop errors (fun () -> Appendix)
        | E (("h1" | "section"), xs)       -> make0 comm prop errors (fun () -> Section (1, seq_of_trees xs))
        | E (("h2" | "subsection"), xs)    -> make0 comm prop errors (fun () -> Section (2, seq_of_trees xs))
        | E (("h3" | "subsubsection"), xs) -> make0 comm prop errors (fun () -> Section (3, seq_of_trees xs))
        | E ("h4", xs)                     -> make0 comm prop errors (fun () -> Section (4, seq_of_trees xs))
        | E ("h5", xs)                     -> make0 comm prop errors (fun () -> Section (5, seq_of_trees xs))
        | E ("h6", xs)                     -> make0 comm prop errors (fun () -> Section (6, seq_of_trees xs))
        | E ("bibliography", xs)           -> make0 ~empty:xs comm prop errors (fun () -> Bibliography)
        | E ("notes", [])                  -> make0 comm prop errors (fun () -> Notes)
        | E ("toc", [])                    -> make0 comm prop errors (fun () -> Toc)
        | E ("title", xs)                  -> make0 comm prop errors (fun () -> Title (1, seq_of_trees xs))
        | E ("subtitle", xs)               -> make0 comm prop errors (fun () -> Title (2, seq_of_trees xs))
        | E ("abstract", xs)               -> make0 comm prop errors (fun () -> Abstract (frag_of_trees xs))
        | E (("rule" | "hr"), xs)          -> make0 ~empty:xs comm prop errors (fun () -> Rule)
        | E ("bib", xs)                    -> make0 comm prop errors (fun () -> Bib (bib_of_trees xs))
        | E ("note", xs)                   -> make0 comm prop errors (fun () -> Note (frag_of_trees xs))
        | E ("newmacro", xs) ->
            make2 comm prop errors (Req "name") (Req "nargs")
                begin fun name nargs ->
                    newmacros := Stringset.add name !newmacros;
                    Macrodef (name, nargs, seq_of_trees xs)
                end
        | E ("newboxout", xs) ->
            make2 comm prop errors (Req "name") (Opt "counter")
                begin fun name counter ->
                    newcustoms := Stringset.add name !newcustoms;
                    Boxoutdef (name, maybe_seq_of_trees xs, counter)
                end
        | E ("newtheorem", xs) ->
            make2 comm prop errors (Req "name") (Opt "counter")
                begin fun name counter ->
                    newcustoms := Stringset.add name !newcustoms;
                    Theoremdef (name, seq_of_trees xs, counter)
                end
        | E (tag, xs) ->
            try
                let (comm, blkpat) = match List.assoc tag block_extdefs with
                    | Syn_empty            -> make0 ~empty:xs comm prop errors (fun () -> Blkpat_empty)
                    | Syn_seq              -> make0 comm prop errors (fun () -> Blkpat_seq (seq_of_trees xs))
                    | Syn_lit              -> make0 comm prop errors (fun () -> Blkpat_lit (literal_of_trees xs))
                    | Syn_frag             -> make0 comm prop errors (fun () -> Blkpat_frag (frag_of_trees xs))
                    | Syn_raw a            -> make1 ~empty:xs comm prop errors (Req a) (fun a -> Blkpat_raw a)
                    | Syn_raw_raw (a1, a2) -> make2 ~empty:xs comm prop errors (Req a1) (Req a2) (fun a1 a2 -> Blkpat_raw_raw (a1, a2))
                    | _                    -> assert false
                in (comm, Extcomm_blk (tag, blkpat))
            with Not_found ->
                if Stringset.mem tag !newcustoms
                then make0 comm prop errors (fun () -> let (s, f) = seq_and_frag_of_trees comm xs in Custom (tag, s, f))
                else if flow
                then raise Inline_in_block
                else (Errors.add errors comm Unknown_tag; (comm, dummy_block))

    and flow_of_trees comm xs =
        try frag_of_trees ~flow:true xs
        with Inline_in_block -> [(comm, Paragraph (seq_of_trees xs))]

    and seq_and_frag_of_trees comm = function
        | [(comm1, prop1, node1)] ->
            cleanup comm1 prop1 errors;
            begin match node1 with
                | E ("dd", xs) ->
                    (None, flow_of_trees comm1 xs)
                | _ ->
                    Errors.(add errors comm Expected_seq_and_frag);
                    (None, [])
            end
        | [(comm1, prop1, node1); (comm2, prop2, node2)] ->
            cleanup comm1 prop1 errors;
            cleanup comm2 prop2 errors;
            begin match (node1, node2) with
                | (E ("dt", xs1), E ("dd", xs2)) ->
                    (Some (seq_of_trees xs1), flow_of_trees comm2 xs2)
                | _ ->
                    Errors.(add errors comm Expected_seq_and_frag);
                    (None, [])
            end
        | _ ->
            Errors.(add errors comm Expected_seq_and_frag);
            (None, [])

    and anon_frags_of_trees xs =
        let f (comm, prop, node) =
            cleanup comm prop errors;
            match node with
                | E ("li", xs) -> (comm, flow_of_trees comm xs)
                | _            -> assert false in
        List.map f xs

    and desc_frags_of_trees comm xs =
        let rev_pairify xs =
            let rec aux accum = function
                | hd1 :: hd2 :: tl -> aux ((hd1, hd2) :: accum) tl
                | []               -> accum
                | _                -> Errors.(add errors comm Bad_dl); [] in
            aux [] xs in
        let f ((comm1, prop1, node1), (comm2, prop2, node2)) =
            cleanup comm1 prop1 errors;
            cleanup comm2 prop2 errors;
            match (node1, node2) with
                | (E ("dt", dts), E ("dd", dds)) ->
                    let dt' = seq_of_trees dts in
                    let dd' = flow_of_trees comm2 dds in
                    (comm1, dt', dd')
                | _ ->
                    Errors.(add errors comm Bad_dl);
                    (comm1, [], []) in
        List.rev_map f (rev_pairify xs)

    and qanda_frags_of_trees xs =
        let f (comm, prop, node) = match node with
            | E ("question", xs)  -> let (s, f) = seq_and_frag_of_trees comm xs in (comm, New_questioner s, f)
            | E ("answer", xs)    -> let (s, f) = seq_and_frag_of_trees comm xs in (comm, New_answerer s, f)
            | E ("rquestion", xs) -> (comm, Same_questioner, flow_of_trees comm xs)
            | E ("ranswer", xs)   -> (comm, Same_answerer, flow_of_trees comm xs)
            | _                   -> assert false in
        List.map f xs

    and tabular_of_trees xs =
        let conv_cells cells =
            let conv_cell (comm, prop, node) =
                cleanup comm prop errors;
                match node with
                    | E (("th" | "td"), []) -> (comm, None)
                    | E (("th" | "td"), xs) -> (comm, Some (seq_of_trees xs))
                    | _                     -> assert false in
            List.map conv_cell cells in
        let conv_rows rows =
            let conv_row (comm, prop, node) =
                cleanup comm prop errors;
                match node with
                    | E ("tr", cells) -> (comm, conv_cells cells)
                    | _               -> assert false in
            List.map conv_row rows in
        let conv_group (thead, tfoot, tbodies) (comm, prop, node) =
            cleanup comm prop errors;
            match node with
                | E ("thead", rows) when thead = None -> (Some (comm, conv_rows rows), tfoot, tbodies)
                | E ("thead", rows)                   -> assert false
                | E ("tfoot", rows) when tfoot = None -> (thead, Some (comm, conv_rows rows), tbodies)
                | E ("tfoot", rows)                   -> assert false
                | E ("tbody", rows)                   -> (thead, tfoot, (comm, conv_rows rows) :: tbodies)
                | _                                   -> assert false in
        let (thead, tfoot, tbodies) = List.fold_left conv_group (None, None, []) xs in
        {thead; tfoot; tbodies = List.rev tbodies}

    and wrapper_of_trees = function
        | [tree] ->
            (None, block_of_tree tree)
        | [tree1; (comm2, prop2, node2)] ->
            cleanup comm2 prop2 errors;
            let seq = match node2 with
                | E ("caption", xs) -> seq_of_trees xs
                | _                 -> assert false in
            (Some seq, block_of_tree tree1)
        | _ ->
            assert false

    and bib_of_trees = function
        | [ (who_comm, who_prop, E ("who", who_xs));
            (what_comm, what_prop, E ("what", what_xs));
            (where_comm, where_prop, E ("where", where_xs))] ->
            let author = cleanup who_comm who_prop errors; (who_comm, seq_of_trees who_xs) in
            let title = cleanup what_comm what_prop errors; (what_comm, seq_of_trees what_xs) in
            let resource = cleanup where_comm where_prop errors; (where_comm, seq_of_trees where_xs) in
            {author; title; resource}
        | _ ->
            assert false in
        
    let ast_of_tree (comm, prop, node) = match node with
        | E ("doc", xs) -> cleanup comm prop errors; frag_of_trees xs
        | _             -> assert false in

    let entity x = match Readconv.Entity_input.expand x with
        | `Okay (_, utf8) -> Some utf8
        | `Error _        -> Some ("&" ^ x ^ ";") in

    let ast = Xmlm.make_input ~entity (`String (0, str)) |> tree_of_input |> ast_of_tree in
    match Errors.to_reading errors with
        | [] -> `Okay ast
        | xs -> `Error xs

