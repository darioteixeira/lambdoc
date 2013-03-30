(********************************************************************************)
(*	Lambcmd.ml
	Copyright (c) 2009-2013 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Eliom_content
open Html5.F
open Options
open Lambdoc_core
open Lambdoc_reader


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

let make_bookmaker ~credential raw_isbns =
	let get_book raw_isbn =
		lwt result =
			try_lwt
				let isbn = Bookaml_ISBN.of_string raw_isbn in
				lwt book = Bookaml_amazon_ocsigen.book_from_isbn_exn ~credential isbn in
				let book' =
					{
					Book.title = book.Bookaml_book.title;
					Book.author = book.Bookaml_book.author;
					Book.publisher = book.Bookaml_book.publisher;
					Book.pubdate = book.Bookaml_book.pubdate;
					}
				in Lwt.return (Bookmaker.Success book')
			with
				| Bookaml_ISBN.Bad_ISBN_length _
				| Bookaml_ISBN.Bad_ISBN_checksum _
				| Bookaml_ISBN.Bad_ISBN_character _ -> Lwt.return (Bookmaker.Failure (Bookmaker.Malformed_ISBN raw_isbn))
				| Bookaml_amazon.No_match _	    -> Lwt.return (Bookmaker.Failure (Bookmaker.Unknown_ISBN raw_isbn))
		in Lwt.return (raw_isbn, result)
	in Lwt_main.run (Lwt_list.map_s get_book raw_isbns)


let string_of_xhtml the_title xhtml =
	let page = (Html5.F.html
			(Html5.F.head
				(Html5.F.title (Html5.F.pcdata the_title))
				[
				Html5.F.css_link ~a:[Html5.F.a_media [`All]; Html5.F.a_title "Default"] ~uri:(Raw.uri_of_string "css/lambdoc.css") ();
				])
			(Html5.F.body [xhtml])) in
	let buf = Buffer.create 1024 in
	Eliom_content.Html5.Printer.print ~output:(Buffer.add_string buf) page;
	Buffer.contents buf


let () =
	let options = Options.parse () in
	let input_str = BatPervasives.input_all options.input_chan in
	let idiosyncrasies = Lambdoc_core.Idiosyncrasies.make
		~max_macro_depth:options.max_macro_depth
		~max_inline_depth:options.max_inline_depth
		~max_block_depth:options.max_block_depth () in
	let bookmaker = match (options.amazon_locale, options.amazon_associate_tag, options.amazon_access_key, options.amazon_secret_key) with
		| (Some locale, Some associate_tag, Some access_key, Some secret_key) ->
			let credential = Bookaml_amazon.make_credential ~locale ~associate_tag ~access_key ~secret_key in
			Some (make_bookmaker ~credential)
		| _ ->
			None in
	let doc = match options.input_markup with
		| `Lambtex  -> Lambdoc_read_lambtex.Main.ambivalent_from_string ?bookmaker ~idiosyncrasies input_str
		| `Lambwiki -> Lambdoc_read_lambwiki.Main.ambivalent_from_string ?bookmaker ~idiosyncrasies input_str
		| `Lambxml -> Lambdoc_read_lambxml.Main.ambivalent_from_string ?bookmaker ~idiosyncrasies input_str
		| `Sexp	    -> Lambdoc_core.Ambivalent.deserialize input_str in
	let output_str = match options.output_markup with
		| `Sexp  -> Lambdoc_core.Ambivalent.serialize doc
		| `Html5 -> string_of_xhtml options.title (Lambdoc_write_html5.Main.write_ambivalent ~translations:options.language doc) in
	output_string options.output_chan output_str;
	options.input_cleaner options.input_chan;
	options.output_cleaner options.output_chan;
	exit (match doc with Ambivalent.Valid _ -> 0 | Ambivalent.Invalid _ -> 3)

