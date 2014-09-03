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

module String = BatString


(********************************************************************************)
(**	{1 Modules}								*)
(********************************************************************************)

module Make_extension (Credential: sig val credential: Bookaml_amazon.credential_t option end) =
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

	type link_t = [ `Book of book_t | `Other of string ] with sexp
	type image_t = unit with sexp
	type extern_t = [ `Book of book_t ] with sexp

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
				cover = match book.Bookaml_book.image_small with Some img -> Some img.url | None -> None;
				} in
			Lwt.return (`Okay (`Book book'))
		with
			| Bookaml_ISBN.Bad_ISBN_length _
			| Bookaml_ISBN.Bad_ISBN_checksum _
			| Bookaml_ISBN.Bad_ISBN_character _ -> Lwt.return (`Error "bad isbn")
			| Bookaml_amazon.No_match _	    -> Lwt.return (`Error "no match")

	let resolve_link href = match (Credential.credential, href) with
		| (Some credential, x) when String.starts_with x "isbn:" -> get_book ~credential (String.lchop ~n:5 x)
		| (_, x)						 -> Lwt.return (`Okay (`Other x))

	let resolve_image _ =
		Lwt.return (`Okay ())

	let resolve_extern href = match (Credential.credential, href) with
		| (Some credential, x) when String.starts_with x "isbn:" -> get_book ~credential (String.lchop ~n:5 x)
		| (_, x)						 -> Lwt.return (`Error "unsupported")

	let expand_link (href, payload) = match payload with
		| `Book book ->
			let href = match book.page with Some page -> page | None -> href in
			Lwt.return (href, Some [Inline.emph [Inline.plain book.title]])
		| `Other href ->
			Lwt.return (href, None)

	let expand_image (href, _) =
		Lwt.return href

	let expand_extern (href, `Book (book : book_t)) =
		let tl =
			[
			Inline.emph [Inline.plain book.title];
			Inline.span [Inline.plain book.author];
			] in
		let xs = match book.cover with
			| Some cover -> Inline.glyph cover "book cover" :: tl
			| None	     -> tl in
		Lwt.return [Block.paragraph xs]
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
				[link ~a:[a_media [`All]; a_title "Default"] ~rel:[`Stylesheet] ~href:(uri_of_string "css/lambdoc.css") ()])
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
	let module Extension = Make_extension (struct let credential = credential end) in
	lwt doc = match options.input_markup with
		| `Lambtex ->
			let module M = Lambdoc_read_lambtex.Main.Make (Extension) in
			M.ambivalent_from_string ~idiosyncrasies input_str
		| `Lambwiki ->
			let module M = Lambdoc_read_lambwiki.Main.Make (Extension) in
			M.ambivalent_from_string ~idiosyncrasies input_str
		| `Lambxml ->
			let module M = Lambdoc_read_lambxml.Main.Make (Extension) in
			M.ambivalent_from_string ~idiosyncrasies input_str
		| `Markdown ->
			let module M = Lambdoc_read_markdown.Main.Make (Extension) in
			M.ambivalent_from_string ~idiosyncrasies input_str
		| `Sexp ->
			Lwt.return (Lambdoc_core.Ambivalent.deserialize Extension.link_t_of_sexp Extension.image_t_of_sexp Extension.extern_t_of_sexp input_str) in
	lwt output_str = match options.output_markup with
		| `Sexp  ->
			Lwt.return (Lambdoc_core.Ambivalent.serialize Extension.sexp_of_link_t Extension.sexp_of_image_t Extension.sexp_of_extern_t doc)
		| `Html5 ->
			let module Html5_writer = Lambdoc_write_html5.Main.Make (Tyxml_backend) (Extension) in
			let valid_options = Html5_writer.({default_valid_options with translations = options.language}) in
			lwt xhtml = Html5_writer.write_ambivalent ~valid_options doc in
			Lwt.return (string_of_xhtml options.title xhtml) in
	output_string options.output_chan output_str;
	options.input_cleaner options.input_chan;
	options.output_cleaner options.output_chan;
	Lwt.return (match doc with Ambivalent.Valid _ -> 0 | Ambivalent.Invalid _ -> 3)


let () =
	main () |> Lwt_main.run |> exit

