(********************************************************************************)
(*	Lambcmd.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Eliom_content.Html5.F
open Options
open Bookaml_amazon
open Lambdoc_core
open Lambdoc_reader


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type processor_t =
	| Manuscript_io of (string -> Lambdoc_core.Ambivalent.manuscript_t) * (Lambdoc_core.Ambivalent.manuscript_t -> string)
	| Composition_io of (string -> Lambdoc_core.Ambivalent.composition_t) * (Lambdoc_core.Ambivalent.composition_t -> string)


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
	let page = (html
			(head
				(title (pcdata the_title)) [])
				(*
				[
				link ~rel:[`Stylesheet] ~href:(Raw.uri_of_string "css/lambdoc.css");
				])
				*)
			(body [xhtml])) in
	let buf = Buffer.create 1024 in
	Eliom_content.Html5.Printer.print ~output:(Buffer.add_string buf) page;
	Buffer.contents buf


let get_processor options =
	let bookmaker = match (options.amazon_locale, options.amazon_associate_tag, options.amazon_access_key, options.amazon_secret_key) with
		| (Some locale, Some associate_tag, Some access_key, Some secret_key) ->
			let credential = Bookaml_amazon.make_credential ~locale ~associate_tag ~access_key ~secret_key in
			Some (make_bookmaker ~credential)
		| _ ->
			None
	in match options.category with
		| `Manuscript ->
			let reader = match options.input_markup with
				| `Lambtex  -> (fun str -> Lambdoc_read_lambtex.Main.ambivalent_manuscript_from_string ?bookmaker str)
				| `Lamblite -> (fun str -> Lambdoc_read_lamblite.Main.ambivalent_manuscript_from_string ?bookmaker str)
				| `Lambhtml -> (fun str -> Lambdoc_read_lambhtml.Main.ambivalent_manuscript_from_string ?bookmaker str)
				| `Sexp	    -> (fun str -> Lambdoc_core.Ambivalent.deserialize_manuscript str)
			and writer = match options.output_markup with
				| `Sexp  -> Lambdoc_core.Ambivalent.serialize_manuscript
				| `Xhtml -> (fun doc -> string_of_xhtml options.title (Lambdoc_write_html5.Main.write_ambivalent_manuscript ~translations:options.language doc))
			in Manuscript_io (reader, writer)
		| `Composition ->
			let reader = match options.input_markup with
				| `Lambtex  -> (fun str -> Lambdoc_read_lambtex.Main.ambivalent_composition_from_string ?bookmaker str)
				| `Lamblite -> (fun str -> Lambdoc_read_lamblite.Main.ambivalent_composition_from_string ?bookmaker str)
				| `Lambhtml -> (fun str -> Lambdoc_read_lambhtml.Main.ambivalent_composition_from_string ?bookmaker str)
				| `Sexp	    -> (fun str -> Lambdoc_core.Ambivalent.deserialize_composition str)
			and writer = match options.output_markup with
				| `Sexp  -> Lambdoc_core.Ambivalent.serialize_composition
				| `Xhtml -> (fun doc -> string_of_xhtml options.title (Lambdoc_write_html5.Main.write_ambivalent_composition ~translations:options.language doc))
			in Composition_io (reader, writer)


let () =
	let options = Options.parse () in
	let input_str = BatStd.input_all options.input_chan in
	let processor = get_processor options in
	let (output_str, is_valid) = match processor with
		| Manuscript_io (reader, writer) ->
			let doc = reader input_str in
			(writer doc, match doc with `Valid _ -> true | _ -> false)
		| Composition_io (reader, writer) ->
			let doc = reader input_str in
			(writer doc, match doc with `Valid _ -> true | _ -> false)
	in
		output_string options.output_chan output_str;
		options.input_cleaner options.input_chan;
		options.output_cleaner options.output_chan;
		exit (if is_valid then 0 else 3)

