(********************************************************************************)
(*  Lambdoc_rmarkdown_readable.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module List = BatList
module String = BatString

open Lambdoc_reader
open Ast


(********************************************************************************)
(*  {1 Private exceptions}                                                      *)
(********************************************************************************)

exception Unsupported_feature of string


(********************************************************************************)
(*  {1 Private type definitions}                                                *)
(********************************************************************************)

type context =
    | Block of Ast.block
    | Inline of Ast.inline
    | Drop


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


let add_style key = function
    | "" -> dummy
    | v  -> {dummy with comm_style = Some (key ^ "=" ^ v)}


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
                    | []              -> List.rev accum
                    | Block blk :: tl -> aux (blk :: accum) tl
                    | Drop :: tl      -> aux accum tl
                    | Inline _ :: _   -> assert false
                end
            | fused ->
                aux ((dummy, Ast.Paragraph fused) :: accum) rest in
    aux [] xs


let inlinify xs =
    let rec aux accum = function
        | []               -> List.rev accum
        | Inline inl :: tl -> aux (inl :: accum) tl
        | Drop :: tl       -> aux accum tl
        | Block _ :: _     -> assert false in
    aux [] xs


let ast_of_omd frag =

    let rec convert_seq seq = inlinify (List.map convert_inline seq)

    and convert_inline = function
        | Omd.Text txt           -> Inline (dummy, Ast.Plain txt)
        | Omd.Emph seq           -> Inline (dummy, Ast.Emph (convert_seq seq))
        | Omd.Bold seq           -> Inline (dummy, Ast.Bold (convert_seq seq))
        | Omd.Code (_, txt)      -> Inline (dummy, Ast.Code [(dummy, Ast.Plain txt)])
        | Omd.Br                 -> Inline (dummy, Ast.Linebreak)
        | Omd.NL                 -> Inline (dummy, Ast.Plain " ")
        | Omd.Url (href, seq, _) -> Inline (dummy, Ast.Link (href, Some (convert_seq seq)))
        | Omd.Ref (ref, link, txt, fallback) ->
            begin match ref#get_ref link with
                | Some (href, _title) -> Inline (dummy, Ast.Link ( href, Some ([(dummy, Ast.Plain txt)])))
                | None                -> Inline (dummy, Ast.Plain fallback#to_string)
            end
        | _ -> assert false

    and convert_frag frag = blockify (List.map convert_block frag)

    and convert_block = function
        | Omd.H1 seq                 -> Block (dummy, Ast.Section (1, convert_seq seq))
        | Omd.H2 seq                 -> Block (dummy, Ast.Section (2, convert_seq seq))
        | Omd.H3 seq                 -> Block (dummy, Ast.Section (3, convert_seq seq))
        | Omd.H4 seq                 -> Block (dummy, Ast.Section (4, convert_seq seq))
        | Omd.H5 seq                 -> Block (dummy, Ast.Section (5, convert_seq seq))
        | Omd.H6 seq                 -> Block (dummy, Ast.Section (6, convert_seq seq))
        | Omd.Paragraph [Omd.Hr]     -> Block (dummy, Ast.Rule)
        | Omd.Paragraph seq          -> Block (dummy, Ast.Paragraph (convert_seq seq))
        | Omd.Ul frags               -> Block (dummy, Ast.Itemize (convert_list frags))
        | Omd.Ol frags               -> Block (dummy, Ast.Enumerate (convert_list frags))
        | Omd.Ulp frags              -> Block (dummy, Ast.Itemize (convert_list frags))
        | Omd.Olp frags              -> Block (dummy, Ast.Enumerate (convert_list frags))
        | Omd.Code_block (lang, txt) -> Block (add_style "lang" lang, Ast.Source txt)
        | Omd.Hr                     -> Block (dummy, Ast.Rule)
        | Omd.NL                     -> Drop
        | Omd.Html _                 -> raise (Unsupported_feature "Html")
        | Omd.Html_block _           -> raise (Unsupported_feature "Html_block")
        | Omd.Html_comment _         -> raise (Unsupported_feature "Html_comment")
        | Omd.Raw _                  -> raise (Unsupported_feature "Raw")
        | Omd.Raw_block _            -> raise (Unsupported_feature "Raw_block")
        | Omd.Blockquote frag        -> Block (dummy, Ast.Quote (convert_frag frag))
        | Omd.Img (alt, src, _title) -> Block (dummy, Ast.Picture (src, alt))
        | Omd.Img_ref (ref, link, txt, fallback) ->
            begin match ref#get_ref link with
                | Some (href, _title) -> Block (dummy, Ast.Picture (href, txt))
                | None                -> Block (dummy, Ast.Paragraph ([dummy, Ast.Plain fallback#to_string]))
            end
        | Omd.X _ -> raise (Unsupported_feature "X")
        | x -> convert_inline x

    and convert_list frags =
        let aux frag = (dummy, convert_frag frag) in
        List.map aux frags

    in convert_frag frag


(********************************************************************************)
(*  {1 Public functions and values}                                             *)
(********************************************************************************)

let ast_from_string ~linenum_offset ~inline_extdefs ~block_extdefs str =
    try
        `Okay (Omd.of_string str |> ast_of_omd)
    with exc ->
        let msg = match exc with
            | Unsupported_feature x -> Printf.sprintf "The document uses an unsupported feature (%s)" x
            | exc                   -> raise exc
        in `Error [(None, None, msg)]

