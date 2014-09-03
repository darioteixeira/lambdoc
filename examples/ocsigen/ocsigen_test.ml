(********************************************************************************)
(*	Ocsigen_test.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Eliom_content.Html5.F


(********************************************************************************)
(**	{1 Modules}								*)
(********************************************************************************)

module Eliom_html5 =
struct
	include Eliom_content.Html5.F.Raw
	module Svg = Eliom_content.Svg.F.Raw
end


module Lambdoc_writer = Lambdoc_write_html5.Make (Eliom_html5) (Lambdoc_writer.Extension.Unit)


module Markup = Litiom_choice.Make
(struct
	type t = [ `Lambtex | `Lambwiki | `Lambxml | `Markdown ]

	let of_string = function
		| "lambtex"  -> `Lambtex
		| "lambwiki" -> `Lambwiki
		| "lambxml"  -> `Lambxml
		| "markdown" -> `Markdown
		| x	     -> invalid_arg ("Markup.of_string: " ^ x)

	let to_string = function
		| `Lambtex  -> "lambtex"
		| `Lambwiki -> "lambwiki"
		| `Lambxml  -> "lambxml"
		| `Markdown -> "markdown"

	let describe = to_string

	let all = [ `Lambtex; `Lambwiki; `Lambxml; `Markdown ]
end)


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

let make_page content =
	let css_uri = make_uri (Eliom_service.static_dir ()) ["css"; "lambdoc.css"] in
	(html
		(head
			(title (pcdata "Lambdoc + Ocsigen"))
			[
			css_link ~a:[(a_media [`All]); (a_title "Default")] ~uri:css_uri ()
			])
		(body content))


let show_fallback_handler () () =
	Lwt.return (make_page [h1 [pcdata "Fallback"]])


let show_fallback_service =
	Eliom_registration.Html5.register_service 
		~path:["show"]
		~get_params:Eliom_parameter.unit
		show_fallback_handler


let show_handler () (markup, source) =
	let reader = match markup with
		| `Lambtex  -> Lambdoc_read_lambtex.Simple.ambivalent_from_string
		| `Lambwiki -> Lambdoc_read_lambwiki.Simple.ambivalent_from_string
		| `Lambxml  -> Lambdoc_read_lambxml.Simple.ambivalent_from_string
		| `Markdown -> Lambdoc_read_markdown.Simple.ambivalent_from_string in
	Lwt.return (make_page [reader source |> Lambdoc_writer.write_ambivalent])


let show_service =
	Eliom_registration.Html5.register_post_service 
		~fallback:show_fallback_service
		~post_params:Eliom_parameter.(Markup.param "param" ** string "source")
		show_handler


let show_form (e_markup, e_source) =
	let sample = "Lorem ipsum dolor sit amet." in
	[
	label ~a:[a_for e_markup] [pcdata "Choose the markup language:"];
	Markup.choose ~name:e_markup ();
	br ();
	label ~a:[a_for e_source] [pcdata "Source:"];
	textarea ~a:[a_rows 8; a_cols 80] ~name:e_source ~value:sample ();
	br ();
	button ~button_type:`Submit [pcdata "Submit"];
	]


let main_handler () () =
	Lwt.return (make_page [post_form show_service show_form ()])


let main_service =
	Eliom_registration.Html5.register_service 
		~path:[""]
		~get_params:Eliom_parameter.unit
		main_handler

