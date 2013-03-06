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

let show_handler sp markup () =
	let (file, reader) = match markup with
		| None
		| Some Lambtex  -> ("sample.lambtex", Lambdoc_read_lambtex.Main.ambivalent_manuscript_from_string)
		| Some Lamblite -> ("sample.lamblite", Lambdoc_read_lamblite.Main.ambivalent_manuscript_from_string)
		| Some Lambhtml -> ("sample.lambhtml", Lambdoc_read_lambhtml.Main.ambivalent_manuscript_from_string) in
	let chan = open_in file in
	let src = Std.input_all chan in
	let () = close_in chan in
	lwt doc = reader src in
	let xhtml = Lambdoc_write_html5.Main.write_ambivalent_manuscript doc
	in Lwt.return (make_page sp [xhtml])


let show_service =
	Eliom_predefmod.Xhtml.register_new_service 
		~path: ["show"]
		~get_params: (Eliom_parameters.radio (Eliom_parameters.user_type ~of_string:markup_of_string ~to_string:string_of_markup) "markup")
		show_handler


let show_form e_markup =
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

