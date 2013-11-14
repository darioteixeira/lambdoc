(********************************************************************************)
(*	Test.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open XHTML.M
open Eliom_parameters
open Bookaml_amazon


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

let book_handler sp (isbn, page) () =
	let content = [p [a ~a:[a_href page] [pcdata ("ISBN " ^ isbn)]]] in
	Lwt.return (make_page sp content)


let book_service =
	Eliom_predefmod.Xhtml.register_new_service 
		~path: ["book"]
		~get_params: (Eliom_parameters.string "isbn" ** Eliom_parameters.user_type uri_of_string string_of_uri "page")
		book_handler


(********************************************************************************)

type markup_t =
	| Lambtex
	| Lambxml

let markup_of_string = function
	| "lambtex"  -> Lambtex
	| "lambxml" -> Lambxml
	| x	     -> invalid_arg x

let string_of_markup = function
	| Lambtex  -> "lambtex"
	| Lambxml -> "lambxml"


(********************************************************************************)

let show_handler sp (markup, (locale, (associate_tag, (access_key, secret_key)))) () =
	let lookup isbn page =
		Eliom_predefmod.Xhtml.make_uri book_service sp (isbn, page) in
	let (book_maker, book_lookup, cover_lookup) = Bookaml_lambdoc.make_helpers ~locale ~associate_tag ~access_key ~secret_key ~lookup ~sp in
	let (file, reader) = match markup with
		| None
		| Some Lambtex  -> ("sample.lambtex", Lambdoc_read_lambtex.Main.ambivalent_manuscript_from_string ~book_maker)
		| Some Lambxml -> ("sample.lambxml", Lambdoc_read_lambxml.Main.ambivalent_manuscript_from_string ~book_maker) in
	let chan = open_in file in
	let src = Std.input_all chan in
	let () = close_in chan in
	lwt doc = reader src in
	let xhtml = Lambdoc_write_html5.Main.write_ambivalent_manuscript ~book_lookup ~cover_lookup doc
	in Lwt.return (make_page sp [xhtml])


let show_service =
	Eliom_predefmod.Xhtml.register_new_service 
		~path: ["show"]
		~get_params:
			(Eliom_parameters.radio (Eliom_parameters.user_type ~of_string:markup_of_string ~to_string:string_of_markup) "markup" **
			Eliom_parameters.user_type ~of_string:Locale.of_string ~to_string:Locale.to_string "locale" **
			Eliom_parameters.string "associate_tag" **
			Eliom_parameters.string "access_key" **
			Eliom_parameters.string "secret_key")
		show_handler


let show_form (e_markup, (e_locale, (e_associate_tag, (e_access_key, e_secret_key)))) =
	[
	XHTML.M.fieldset
		[
		XHTML.M.p [pcdata "Choose the markup language:"];

		Eliom_predefmod.Xhtml.user_type_radio ~a:[a_id "e_lambtex"] string_of_markup ~name:e_markup ~value:Lambtex ();
		XHTML.M.label ~a:[a_for "e_lambtex"] [pcdata "Lambtex"];
		XHTML.M.br ();

		Eliom_predefmod.Xhtml.user_type_radio ~a:[a_id "e_lambxml"] string_of_markup ~name:e_markup ~value:Lambxml ();
		XHTML.M.label ~a:[a_for "e_lambxml"] [pcdata "Lambxml"];
		XHTML.M.br ();

		XHTML.M.p [pcdata "Enter your Amazon credentials:"];

		XHTML.M.label ~a:[a_for "e_locale"] [pcdata "Locale:"];
		Eliom_predefmod.Xhtml.user_type_input Locale.to_string ~a:[a_id "e_locale"] ~input_type:`Text ~name:e_locale ~value:`US ();
		XHTML.M.br ();

		XHTML.M.label ~a:[a_for "e_associate_tag"] [pcdata "Associate tag:"];
		Eliom_predefmod.Xhtml.string_input ~a:[a_id "e_associate_tag"] ~input_type:`Text ~name:e_associate_tag ();
		XHTML.M.br ();

		XHTML.M.label ~a:[a_for "e_access_key"] [pcdata "Access key:"];
		Eliom_predefmod.Xhtml.string_input ~a:[a_id "e_access_key"] ~input_type:`Text ~name:e_access_key ();
		XHTML.M.br ();

		XHTML.M.label ~a:[a_for "e_secret_key"] [pcdata "Secret key:"];
		Eliom_predefmod.Xhtml.string_input ~a:[a_id "e_secret_key"] ~input_type:`Text ~name:e_secret_key ();
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

