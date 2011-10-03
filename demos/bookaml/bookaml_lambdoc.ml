(********************************************************************************)
(*	Bookaml_lambdoc.ml
	Copyright (c) 2010-2011 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core
open Bookaml_amazon


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

let make_helpers ~locale ~associate_tag ~access_key ~secret_key ~lookup ~sp =
	let credential = Bookaml_amazon.make_credential ~locale ~associate_tag ~access_key ~secret_key in
	let books = Hashtbl.create 0 in
	let book_maker isbn =
		try_lwt
			let isbn' = ISBN.of_string isbn in
			lwt book = Bookaml_amazon.book_from_isbn_exn ~credential isbn' in
			let () = Hashtbl.add books isbn book in
			let book' =
				{
				Book.title = book.bk_title;
				Book.author = book.bk_author;
				Book.publisher = book.bk_publisher;
				Book.pubdate = book.bk_pubdate;
				}
			in Lwt.return book'
		with
			| ISBN.Bad_ISBN_length _
			| ISBN.Bad_ISBN_checksum _
			| ISBN.Bad_ISBN_character _ -> Lwt.fail (Book.Malformed_ISBN isbn)
			| Bookaml_amazon.No_match _ -> Lwt.fail (Book.Unknown_ISBN isbn)
	and book_lookup isbn =
		let book = Hashtbl.find books isbn in
		lookup isbn book.bk_page
	and cover_lookup isbn cover =
		let book = Hashtbl.find books isbn in
		let image = match cover with
			| Book.Small  -> book.bk_image_small
			| Book.Medium -> book.bk_image_medium
			| Book.Large  -> book.bk_image_large
		in image.bk_url
	in (book_maker, book_lookup, cover_lookup)

