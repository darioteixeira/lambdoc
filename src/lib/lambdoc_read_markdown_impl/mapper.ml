(********************************************************************************)
(*	Mapper.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_reader
open Ast

module List = BatList
module String = BatString


(********************************************************************************)
(*	{1 Exceptions}								*)
(********************************************************************************)

exception Unsupported_feature of string


(********************************************************************************)
(*	{1 Type definitions}							*)
(********************************************************************************)

type context_t =
	| Block of Ast.block_t
	| Inline of Ast.inline_t
	| Drop


(********************************************************************************)
(*	{1 Private functions and values}					*)
(********************************************************************************)

let dummy =
	{
	comm_tag = None;
	comm_label = None;
	comm_order = None;
	comm_style = None;
	comm_linenum = 0;
	}


let add_style key = function
	| "" -> dummy
	| v  -> {dummy with comm_style = Some (key ^ "=" ^ v)}


let blockify xs =
	let rec aux accum xs =
		let rec fuse accum = function
			| Inline inl :: tl	      -> fuse (inl :: accum) tl
			| Drop :: tl when accum <> [] -> fuse ((dummy, Ast.Plain " ") :: accum) tl
			| xs			      -> (List.rev accum, xs) in
		let (fused, rest) = fuse [] xs in
		match fused with
			| [] ->
				begin match rest with
					| []		  -> List.rev accum
					| Block blk :: tl -> aux (blk :: accum) tl
					| Drop :: tl	  -> aux accum tl
					| Inline _ :: _   -> assert false
				end
			| fused ->
				aux ((dummy, Ast.Paragraph fused) :: accum) rest in
	aux [] xs


let inlinify xs =
	let rec aux accum = function
		| []		   -> List.rev accum
		| Inline inl :: tl -> aux (inl :: accum) tl
		| Drop :: tl	   -> aux accum tl
		| Block _ :: _	   -> assert false in
	aux [] xs


(********************************************************************************)
(*	{1 Public functions and values}						*)
(********************************************************************************)

let ast_of_omd frag =

	let rec convert_seq seq = inlinify (List.map convert_inline seq)

	and convert_inline = function
		| Omd.Text txt			-> Inline (dummy, Ast.Plain txt)
		| Omd.Emph seq			-> Inline (dummy, Ast.Emph (convert_seq seq))
		| Omd.Bold seq			-> Inline (dummy, Ast.Bold (convert_seq seq))
		| Omd.Code (_, txt)		-> Inline (dummy, Ast.Code [(dummy, Ast.Plain txt)])
		| Omd.Br			-> Inline (dummy, Ast.Linebreak)
		| Omd.NL			-> Inline (dummy, Ast.Plain " ")
		| Omd.Url (href, seq, _)	-> Inline (dummy, Ast.Link (href, Some (convert_seq seq)))
		| Omd.Ref (ref, link, txt, fallback)
			-> begin match ref#get_ref link with
			   | Some (href, _title) -> Inline (dummy, Ast.Link ( href, Some ([(dummy, Ast.Plain txt)])))
			   | None -> Inline (dummy, Ast.Plain fallback#to_string)
			end
		| _				-> assert false

	and convert_frag frag = blockify (List.map convert_block frag)

	and convert_block = function
		| Omd.H1 seq			-> Block (dummy, Ast.Section (`Level1, convert_seq seq))
		| Omd.H2 seq			-> Block (dummy, Ast.Section (`Level2, convert_seq seq))
		| Omd.H3 seq			-> Block (dummy, Ast.Section (`Level3, convert_seq seq))
		| Omd.H4 seq			-> Block (dummy, Ast.Section (`Level4, convert_seq seq))
		| Omd.H5 seq			-> Block (dummy, Ast.Section (`Level5, convert_seq seq))
		| Omd.H6 seq			-> Block (dummy, Ast.Section (`Level6, convert_seq seq))
		| Omd.Paragraph [Omd.Hr]	-> Block (dummy, Ast.Rule)
		| Omd.Paragraph seq		-> Block (dummy, Ast.Paragraph (convert_seq seq))
		| Omd.Ul frags			-> Block (dummy, Ast.Itemize (convert_list frags))
		| Omd.Ol frags			-> Block (dummy, Ast.Enumerate (convert_list frags))
		| Omd.Ulp frags			-> Block (dummy, Ast.Itemize (convert_list frags))
		| Omd.Olp frags			-> Block (dummy, Ast.Enumerate (convert_list frags))
		| Omd.Code_block (lang, txt)	-> Block (add_style "lang" lang, Ast.Source txt)
		| Omd.Hr			-> Block (dummy, Ast.Rule)
		| Omd.NL			-> Drop
		| Omd.Img_ref _			-> raise (Unsupported_feature "Img_ref")
		| Omd.Html _			-> raise (Unsupported_feature "Html")
		| Omd.Html_block _		-> raise (Unsupported_feature "Html_block")
		| Omd.Html_comment _		-> raise (Unsupported_feature "Html_comment")
		| Omd.Raw _			-> raise (Unsupported_feature "Raw")
		| Omd.Raw_block _		-> raise (Unsupported_feature "Raw_block")
		| Omd.Blockquote frag		-> Block (dummy, Ast.Quote (convert_frag frag))
		| Omd.Img _			-> raise (Unsupported_feature "Img")
		| Omd.X _			-> raise (Unsupported_feature "X")
		| x				-> convert_inline x

	and convert_list frags =
		let aux frag = (dummy, convert_frag frag) in
		List.map aux frags

	in convert_frag frag

