open XHTML.M

(* --------------------------------------------------------------------	*)
(* Service "coucou".							*)
(* --------------------------------------------------------------------	*)

let src = IO.read_all (IO.input_channel (open_in "tutorial.ltex"))

let coucou_handler sp () () =
	let css_uri = Eliom_predefmod.Xhtml.make_uri (Eliom_services.static_dir sp) sp ["css"; "lambdoc.css"] in
	let doc = Lambdoc.Lambtex_reader.ambivalent_manuscript_from_string src in
	let xhtml = Lambdoc.Xhtml_writer.ambivalent_manuscript_to_xhtml doc in
	Lwt.return
		(html
			(head ~a:[a_profile (uri_of_string "http://www.w3.org/2005/11/profile")]
				(title (pcdata "Test story"))
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

