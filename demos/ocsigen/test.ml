(********************************************************************************)
(*	Test.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open XHTML.M
open Eliom_parameters
open Lambdoc_core
open Features
open Bookaml_amazon


(********************************************************************************)

let book_maker ~associate_tag ~access_key ~secret_key ~locale raw_isbn =
	try_lwt
		let isbn = ISBN.of_string raw_isbn in
		lwt book = Bookaml_amazon.book_from_isbn_exn ~associate_tag ~access_key ~secret_key ~locale isbn in
		let data =
			{
			Lambdoc_core.Book.title = book.Bookaml_amazon.title;
			Lambdoc_core.Book.author = book.Bookaml_amazon.author;
			Lambdoc_core.Book.publisher = book.Bookaml_amazon.publisher;
			Lambdoc_core.Book.year = book.Bookaml_amazon.year;
			}
		in Lwt.return (raw_isbn, data)
	with
		| ISBN.Bad_ISBN_length _
		| ISBN.Bad_ISBN_checksum _
		| ISBN.Bad_ISBN_character _ -> Lwt.fail (Lambdoc_core.Book.Malformed_ISBN raw_isbn)
		| Bookaml_amazon.No_match _ -> Lwt.fail (Lambdoc_core.Book.Unknown_ISBN raw_isbn)


(********************************************************************************)

type markup_t =
	| Lambtex
	| Lamblite
	| Lambhtml

let markup_of_string = function
	| "lambtex"  -> Lambtex
	| "lamblite" -> Lamblite
	| "lambhtml" -> Lambhtml
	| x	     -> invalid_arg x

let string_of_markup = function
	| Lambtex  -> "lambtex"
	| Lamblite -> "lamblite"
	| Lambhtml -> "lambhtml"


(********************************************************************************)

let make_page sp content =
	let css_uri = Eliom_predefmod.Xhtml.make_uri (Eliom_services.static_dir sp) sp ["css"; "lambdoc.css"]
	in (XHTML.M.html
		(XHTML.M.head ~a:[a_profile (uri_of_string "http://www.w3.org/2005/11/profile")]
			(XHTML.M.title (pcdata "Lambdoc + Ocsigen"))
			[
			XHTML.M.meta ~a: [a_http_equiv "content-type"] ~content: "text/html; charset=utf-8" ();
			Eliom_predefmod.Xhtml.css_link ~a:[(a_media [`All]); (a_title "Default")] ~uri:css_uri ()
			])
		(XHTML.M.body content))


(********************************************************************************)

let show_handler sp (markup, (associate_tag, (access_key, (secret_key, locale))))  () =
	let (file, reader) = match markup with
		| None
		| Some Lambtex  -> ("sample.lambtex", Lambdoc_read_lambtex.Main.ambivalent_manuscript_from_string)
		| Some Lamblite -> ("sample.lamblite", Lambdoc_read_lamblite.Main.ambivalent_manuscript_from_string)
		| Some Lambhtml -> ("sample.lambhtml", Lambdoc_read_lambhtml.Main.ambivalent_manuscript_from_string) in
	let chan = open_in file in
	let src = Std.input_all chan in
	let () = close_in chan in
	lwt doc = reader src in
	let xhtml = Lambdoc_write_xhtml.Main.write_ambivalent_manuscript doc
	in Lwt.return (make_page sp [xhtml])


let show_service =
	Eliom_predefmod.Xhtml.register_new_service 
		~path: ["show"]
		~get_params:
			(Eliom_parameters.radio (Eliom_parameters.user_type ~of_string:markup_of_string ~to_string:string_of_markup) "markup" **
			Eliom_parameters.string "associate_tag" **
			Eliom_parameters.string "access_key" **
			Eliom_parameters.string "secret_key" **
			Eliom_parameters.user_type ~of_string:Locale.of_string ~to_string:Locale.to_string "locale")
		show_handler


let show_form (e_markup, (e_associate_tag, (e_access_key, (e_secret_key, e_locale)))) =
	[
	XHTML.M.fieldset
		[
		XHTML.M.p [pcdata "Choose the markup language:"];

		Eliom_predefmod.Xhtml.user_type_radio ~a:[a_id "e_lambtex"] string_of_markup ~name:e_markup ~value:Lambtex ();
		XHTML.M.label ~a:[a_for "e_lambtex"] [pcdata "Lambtex"];
		XHTML.M.br ();

		Eliom_predefmod.Xhtml.user_type_radio ~a:[a_id "e_lamblite"] string_of_markup ~name:e_markup ~value:Lamblite ();
		XHTML.M.label ~a:[a_for "e_lamblite"] [pcdata "Lamblite"];
		XHTML.M.br ();

		Eliom_predefmod.Xhtml.user_type_radio ~a:[a_id "e_lambhtml"] string_of_markup ~name:e_markup ~value:Lambhtml ();
		XHTML.M.label ~a:[a_for "e_lambhtml"] [pcdata "Lambhtml"];
		XHTML.M.br ();

		XHTML.M.p [pcdata "Enter your Amazon credentials:"];

		XHTML.M.label ~a:[a_for "e_associate_tag"] [pcdata "Associate tag:"];
		Eliom_predefmod.Xhtml.string_input ~a:[a_id "e_associate_tag"] ~input_type:`Text ~name:e_associate_tag ();
		XHTML.M.br ();

		XHTML.M.label ~a:[a_for "e_access_key"] [pcdata "Access key:"];
		Eliom_predefmod.Xhtml.string_input ~a:[a_id "e_access_key"] ~input_type:`Text ~name:e_access_key ();
		XHTML.M.br ();

		XHTML.M.label ~a:[a_for "e_secret_key"] [pcdata "Secret key:"];
		Eliom_predefmod.Xhtml.string_input ~a:[a_id "e_secret_key"] ~input_type:`Text ~name:e_secret_key ();
		XHTML.M.br ();

		XHTML.M.label ~a:[a_for "e_locale"] [pcdata "Locale:"];
		Eliom_predefmod.Xhtml.user_type_input Locale.to_string ~a:[a_id "e_locale"] ~input_type:`Text ~name:e_locale ();
		XHTML.M.br ();

		Eliom_predefmod.Xhtml.string_input ~input_type:`Submit ~value:"Submit" ();
		]
	]


(********************************************************************************)

let main_handler sp () () =
	let form = Eliom_predefmod.Xhtml.get_form show_service sp show_form
	in Lwt.return (make_page sp [form])

let main_service =
	Eliom_predefmod.Xhtml.register_new_service 
		~path: [""]
		~get_params: Eliom_parameters.unit
		main_handler

