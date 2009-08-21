open XHTML.M
open Lambdoc_core
open Features

let test_handler sp () () =
	let ch = open_in "complete.lite" in
	let src = Std.input_all ch in
	let () = close_in ch in
	let css_uri = Eliom_predefmod.Xhtml.make_uri (Eliom_services.static_dir sp) sp ["css"; "lambdoc.css"] in
	let accept_list = [ ] in
	let deny_list = [ ] in
	let default : default_t = `Accept in
	let () = Ocsigen_messages.warning "Starting parsing..." in
	let doc = Lambdoc_read_lamblite.Main.ambivalent_composition_from_string ~verify_utf8:false ~accept_list ~deny_list ~default src in
	let () = Ocsigen_messages.warning "Finished parsing!" in
	let ser = Lambdoc_core.Ambivalent.serialize_composition doc in
	let () = Ocsigen_messages.warning ser in
	let xhtml = Lambdoc_write_xhtml.Main.write_ambivalent_composition doc in
	Lwt.return
		(html
			(head ~a:[a_profile (uri_of_string "http://www.w3.org/2005/11/profile")]
				(title (pcdata "Simple sample of Lambtex"))
				[
				meta ~a: [a_http_equiv "content-type"] ~content: "text/html; charset=utf-8" ();
				Eliom_predefmod.Xhtml.css_link ~a:[(a_media [`All]); (a_title "Default")] ~uri:css_uri ()
				])
			(body [xhtml]))

let test_service =
	Eliom_predefmod.Xhtml.register_new_service 
		~path: [""]
		~get_params: Eliom_parameters.unit
		test_handler

