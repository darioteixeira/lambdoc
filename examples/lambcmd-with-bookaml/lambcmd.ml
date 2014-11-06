(********************************************************************************)
(*	Lambcmd.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Sexplib.Std
open Options
open Lambdoc_core
open Extcomm

module String = BatString


(********************************************************************************)
(**	{1 Modules}								*)
(********************************************************************************)

module Extension =
struct
	module Monad = struct include Lwt let iter = Lwt_list.iter_s end

	type book_t =
		{
		title: string;
		author: string;
		publisher: string;
		pubdate: string option;
		page: string option;
		cover: string option;
		} with sexp

	type linkdata_t = [ `Book of book_t | `Other of string ] with sexp
	type imagedata_t = unit with sexp
	type extinldata_t = unit with sexp
	type extblkdata_t = [ `Book of book_t ] with sexp
	type rconfig_t = Bookaml_amazon.credential_t
	type wconfig_t = unit

	let extinldefs = []

	let extblkdefs =
		[
		("bookpic", (`Synblk_simraw, [`Embeddable_blk; `Figure_blk]));
		]

	let get_book ~credential raw_isbn =
		try_lwt
			let isbn = Bookaml_ISBN.of_string raw_isbn in
			lwt book = Bookaml_amazon_ocsigen.book_from_isbn_exn ~credential isbn in
			let book' =
				{
				title = book.Bookaml_book.title;
				author = book.Bookaml_book.author;
				publisher = book.Bookaml_book.publisher;
				pubdate = book.Bookaml_book.pubdate;
				page = book.Bookaml_book.page;
				cover = match book.Bookaml_book.image_small with Some img -> Some img.Bookaml_book.url | None -> None;
				} in
			Lwt.return (`Okay (`Book book'))
		with
			| Bookaml_ISBN.Bad_ISBN_length _
			| Bookaml_ISBN.Bad_ISBN_checksum _
			| Bookaml_ISBN.Bad_ISBN_character _ -> Lwt.return (`Error (`Failed "bad isbn"))
			| Bookaml_amazon.No_match _	    -> Lwt.return (`Error (`Failed "no match"))

	let read_link ?rconfig href = match (rconfig, href) with
		| (Some credential, x) when String.starts_with x "isbn:" -> get_book ~credential (String.lchop ~n:5 x)
		| (_, x)						 -> Lwt.return (`Okay (`Other x))

	let read_image ?rconfig _ =
		Lwt.return (`Okay ())

	let read_extinl ?rconfig _ _ =
		assert false	(* This should never be called *)

	let read_extblk ?rconfig tag extcomm = match (rconfig, tag, extcomm) with
		| (Some credential, "bookpic", Extblk_simraw txt) ->
			if String.starts_with txt "isbn:"
			then get_book ~credential (String.lchop ~n:5 txt)
			else Lwt.return (`Error `Unsupported)
		| _ ->
			Lwt.return (`Error `Unsupported)

	let write_link ?wconfig href = function
		| `Book book ->
			let href = match book.page with Some page -> page | None -> href in
			Lwt.return (href, Some [Inline.emph [Inline.plain book.title]])
		| `Other href ->
			Lwt.return (href, None)

	let write_image ?wconfig href _ =
		Lwt.return href

	let write_extinl ?wconfig _ _ _ =
		assert false	(* This should never be called *)

	let write_extblk ?wconfig tag _ data = match (tag, data) with
		| ("bookpic", `Book (book : book_t)) ->
			let tl =
				[
				Inline.emph [Inline.plain book.title];
				Inline.span [Inline.plain book.author];
				] in
			let xs = match book.cover with
				| Some cover -> Inline.glyph cover "book cover" :: tl
				| None	     -> tl in
			Lwt.return [Block.paragraph xs]
		| _ ->
			assert false	(* This should never be called *)
end


module Tyxml_backend =
struct
	include Html5.M
	module Svg = Svg.M
end


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

let string_of_xhtml the_title xhtml =
	let open Html5.M in
	let page = (html
			(head
				(title (pcdata the_title))
				[
				meta ~a:[a_charset "utf-8"] ();
				link ~a:[a_media [`All]; a_title "Default"] ~rel:[`Stylesheet] ~href:(uri_of_string "css/lambdoc.css") ()
				])
			(body [xhtml])) in
	let buf = Buffer.create 1024 in
	Html5.P.print ~output:(Buffer.add_string buf) page;
	Buffer.contents buf


let main () =
	let options = Options.parse () in
	let input_str = BatPervasives.input_all options.input_chan in
	let idiosyncrasies =
		let base =
			if options.unrestricted
			then Idiosyncrasies.unrestricted
			else Idiosyncrasies.default in
		{
		base with
			Idiosyncrasies.max_macro_depth = options.max_macro_depth;
			Idiosyncrasies.max_inline_depth = options.max_inline_depth;
			Idiosyncrasies.max_block_depth = options.max_block_depth;
		} in
	let credential = match (options.amazon_locale, options.amazon_associate_tag, options.amazon_access_key, options.amazon_secret_key) with
		| (Some locale, Some associate_tag, Some access_key, Some secret_key) ->
			Some (Bookaml_amazon.make_credential ~locale ~associate_tag ~access_key ~secret_key)
		| _ ->
			None in
	lwt doc = match options.input_markup with
		| `Lambtex ->
			let module M = Lambdoc_read_lambtex.Make (Extension) in
			M.ambivalent_from_string ?rconfig:credential ~idiosyncrasies input_str
		| `Lambwiki ->
			let module M = Lambdoc_read_lambwiki.Make (Extension) in
			M.ambivalent_from_string ?rconfig:credential ~idiosyncrasies input_str
		| `Lambxml ->
			let module M = Lambdoc_read_lambxml.Make (Extension) in
			M.ambivalent_from_string ?rconfig:credential ~idiosyncrasies input_str
		| `Markdown ->
			let module M = Lambdoc_read_markdown.Make (Extension) in
			M.ambivalent_from_string ?rconfig:credential ~idiosyncrasies input_str
		| `Sexp ->
			Lwt.return (Lambdoc_core.Ambivalent.deserialize Extension.linkdata_t_of_sexp Extension.imagedata_t_of_sexp Extension.extinldata_t_of_sexp Extension.extblkdata_t_of_sexp input_str) in
	lwt output_str = match options.output_markup with
		| `Sexp  ->
			Lwt.return (Lambdoc_core.Ambivalent.serialize Extension.sexp_of_linkdata_t Extension.sexp_of_imagedata_t Extension.sexp_of_extinldata_t Extension.sexp_of_extblkdata_t doc)
		| `Html5 ->
			let module Html5_writer = Lambdoc_write_html5.Make (Extension) (Tyxml_backend) in
			let valid_options = Html5_writer.({default_valid_options with translations = options.language}) in
			lwt xhtml = Html5_writer.write_ambivalent ~valid_options doc in
			Lwt.return (string_of_xhtml options.title xhtml) in
	output_string options.output_chan output_str;
	options.input_cleaner options.input_chan;
	options.output_cleaner options.output_chan;
	Lwt.return (match doc with Ambivalent.Valid _ -> 0 | Ambivalent.Invalid _ -> 3)


let () =
	main () |> Lwt_main.run |> exit

