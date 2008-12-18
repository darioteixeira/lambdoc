open XHTML.M
open Lambdoc_reader
open Features

(* --------------------------------------------------------------------	*)
(* Service "coucou".							*)
(* --------------------------------------------------------------------	*)

let src = IO.read_all (IO.input_channel (open_in "complete.ltex"))

let coucou_handler sp () () =
	let css_uri = Eliom_predefmod.Xhtml.make_uri (Eliom_services.static_dir sp) sp ["css"; "lambdoc.css"] in
	let accept_list = [ ] in
	let deny_list = [ ] in
	let default : default_t = `Accept in
	let doc = Read_lambtex.Main.ambivalent_manuscript_from_string ~accept_list ~deny_list ~default src in
	let xhtml = Write_xhtml.Main.write_ambivalent_manuscript doc in
	Lwt.return
		(html
			(head ~a:[a_profile (uri_of_string "http://www.w3.org/2005/11/profile")]
				(title (pcdata "Lambtex Tutorial"))
				[
				meta ~a: [a_http_equiv "content-type"] ~content: "text/html; charset=utf-8" ();
				Eliom_predefmod.Xhtml.css_link ~a:[(a_media [`All]); (a_title "Default")] ~uri:css_uri ()
				])
			(body [xhtml]))

let coucou_service =
	Eliom_predefmod.Xhtml.register_new_service 
		~path: [""]
		~get_params: Eliom_parameters.unit
		coucou_handler

