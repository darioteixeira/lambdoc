(********************************************************************************)
(*  Lambdoc_rmarkdown_readable.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_prelude
open Lambdoc_reader
open Ast


(********************************************************************************)
(*  {1 Private exceptions}                                                      *)
(********************************************************************************)

exception Unsupported_raw of string
exception Unsupported_block of Omd.element
exception Unsupported_inline of Omd.element


(********************************************************************************)
(*  {1 Private type definitions}                                                *)
(********************************************************************************)

type context =
    | Block of Ast.block
    | Multiblock of Ast.block list
    | Inline of Ast.inline
    | Drop


(********************************************************************************)
(*  {1 Public type definitions}                                                 *)
(********************************************************************************)

type options = unit


(********************************************************************************)
(*  {1 Private functions and values}                                            *)
(********************************************************************************)

let dummy =
    {
    comm_tag = None;
    comm_label = None;
    comm_order = None;
    comm_style = None;
    comm_linenum = 0;
    comm_originator = Source;
    }


let get_entity =
    let rex = Re.(compile (seq [bos; char '&'; group (rep1 any); char ';'; eos])) in
    fun str ->
        let groups = Re.exec rex str in
        if Re.test groups 1
        then Some (Re.get groups 1)
        else None
                                 

let add_style key = function
    | "" -> dummy
    | v  -> {dummy with comm_style = Some (key ^ "=" ^ v)}


let title_of_string = function
    | "" -> None
    | x  -> Some x


let blockify xs =
    let rec aux accum xs =
        let rec fuse accum = function
            | Inline inl :: tl            -> fuse (inl :: accum) tl
            | Drop :: tl when accum <> [] -> fuse ((dummy, Ast.Plain " ") :: accum) tl
            | xs                          -> (List.rev accum, xs) in
        let (fused, rest) = fuse [] xs in
        match fused with
            | [] ->
                begin match rest with
                    | []                    -> List.rev accum
                    | Block blk :: tl       -> aux (blk :: accum) tl
                    | Multiblock blks :: tl -> aux (blks @ accum) tl
                    | Drop :: tl            -> aux accum tl
                    | Inline _ :: _         -> assert false
                end
            | fused ->
                aux ((dummy, Ast.Paragraph fused) :: accum) rest in
    aux [] xs


let inlinify xs =
    let rec aux accum = function
        | []                            -> List.rev accum
        | Inline inl :: tl              -> aux (inl :: accum) tl
        | Drop :: tl                    -> aux accum tl
        | (Block _ | Multiblock _) :: _ -> assert false in
    aux [] xs


let ast_of_omd frag =

    let rec convert_seq seq = inlinify (List.map convert_inline seq)

    and convert_inline = function
        | Omd.Text txt ->
            Inline (dummy, Ast.Plain txt)
        | Omd.Emph seq ->
            Inline (dummy, Ast.Emph (convert_seq seq))
        | Omd.Bold seq ->
            Inline (dummy, Ast.Bold (convert_seq seq))
        | Omd.Code (lang, txt) ->
            Inline (add_style "lang" lang, Ast.Code txt)
        | Omd.Br ->
            Inline (dummy, Ast.Linebreak)
        | Omd.NL ->
            Inline (dummy, Ast.Plain " ")
        | Omd.Url (href, seq, _) ->
            Inline (dummy, Ast.Link (href, Some (convert_seq seq)))
        | Omd.Ref (ref, link, txt, fallback) ->
            begin match ref#get_ref link with
                | Some (href, _title) -> Inline (dummy, Ast.Link ( href, Some ([(dummy, Ast.Plain txt)])))
                | None                -> Inline (dummy, Ast.Plain fallback#to_string)
            end
        | Omd.Raw raw ->
            begin match get_entity raw with
                | Some ent -> Inline (dummy, Ast.Entity ent)
                | None     -> raise (Unsupported_raw raw)
            end
        | Omd.Img (alt, src, title) ->
            Inline (dummy, Ast.Glyph (src, alt, title_of_string title))
        | x ->
            raise (Unsupported_inline x)

    and convert_paragraph accum seq = function
        | [] ->
            begin match seq with
                | [] -> accum
                | seq -> (dummy, Ast.Paragraph (convert_seq (List.rev seq))) :: accum
            end
        | Omd.Hr :: tl ->
            let rule = (dummy, Ast.Rule) in
            let accum = match seq with
                | []  -> rule :: accum
                | seq -> rule :: (dummy, Ast.Paragraph (convert_seq (List.rev seq))) :: accum in
            convert_paragraph accum [] tl
        | hd :: tl ->
            convert_paragraph accum (hd :: seq) tl

    and convert_frag frag = blockify (List.map convert_block frag)

    and convert_block = function
        | Omd.H1 seq ->
            Block (dummy, Ast.Section (1, convert_seq seq))
        | Omd.H2 seq ->
            Block (dummy, Ast.Section (2, convert_seq seq))
        | Omd.H3 seq ->
            Block (dummy, Ast.Section (3, convert_seq seq))
        | Omd.H4 seq ->
            Block (dummy, Ast.Section (4, convert_seq seq))
        | Omd.H5 seq ->
            Block (dummy, Ast.Section (5, convert_seq seq))
        | Omd.H6 seq ->
            Block (dummy, Ast.Section (6, convert_seq seq))
        | Omd.Paragraph xs ->
            Multiblock (convert_paragraph [] [] xs)
        | Omd.Ul frags ->
            Block (dummy, Ast.Itemize (convert_list frags))
        | Omd.Ol frags ->
            Block (dummy, Ast.Enumerate (convert_list frags))
        | Omd.Ulp frags ->
            Block (dummy, Ast.Itemize (convert_list frags))
        | Omd.Olp frags ->
            Block (dummy, Ast.Enumerate (convert_list frags))
        | Omd.Code_block (lang, txt) ->
            Block (add_style "lang" lang, Ast.Source txt)
        | Omd.Hr ->
            Block (dummy, Ast.Rule)
        | Omd.NL ->
            Drop
        | Omd.Blockquote frag ->
            Block (dummy, Ast.Quote (convert_frag frag))
        | Omd.Img (alt, src, title) ->
            Block (dummy, Ast.Picture (src, alt, title_of_string title))
        | Omd.Img_ref (ref, link, alt, fallback) ->
            begin match ref#get_ref link with
                | Some (href, title) -> Block (dummy, Ast.Picture (href, alt, title_of_string title))
                | None               -> Block (dummy, Ast.Paragraph ([dummy, Ast.Plain fallback#to_string]))
            end
        | (Omd.Html _ | Omd.Html_block _ | Omd.Html_comment _ | Omd.Raw _ | Omd.Raw_block _ | Omd.X _) as x ->
            raise (Unsupported_block x)
        | x ->
            convert_inline x

    and convert_list frags =
        let aux frag = (dummy, convert_frag frag) in
        List.map aux frags

    in convert_frag frag


(********************************************************************************)
(*  {1 Public functions and values}                                             *)
(********************************************************************************)

let ast_from_string ?options ~linenum_offset ~inline_extdefs ~block_extdefs str =
    try
        `Okay (Omd.of_string str |> ast_of_omd)
    with exc ->
        let msg = match exc with
            | Unsupported_raw x    -> Printf.sprintf "Cannot handle raw '%s' in inline context" x
            | Unsupported_block x  -> Printf.sprintf "Cannot handle '%s' in block context" (Omd.to_markdown [x])
            | Unsupported_inline x -> Printf.sprintf "Cannot handle '%s' in inline context" (Omd.to_markdown [x])
            | exc                  -> raise exc
        in `Error [(None, None, msg)]

