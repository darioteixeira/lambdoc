(********************************************************************************)
(*	Lambtex_manual.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open XHTML.M


let main_handler sp () () =
        let ch = open_in "lambtex_manual.ltex" in
        let src = Std.input_all ch in
        let () = close_in ch in
	let css_uri = Eliom_predefmod.Xhtml.make_uri (Eliom_services.static_dir sp) sp ["css"; "lambdoc.css"] in
	let time1 = Unix.gettimeofday () in
	let doc = Lambdoc_read_lambtex.Main.ambivalent_manuscript_from_string src in
	let time2 = Unix.gettimeofday () in
	let xhtml = Lambdoc_write_xhtml.Main.write_ambivalent_manuscript doc in
	let time3 = Unix.gettimeofday () in
	Ocsigen_messages.warning (Printf.sprintf "Parsing took %.3f secs" (time2 -. time1));
	Ocsigen_messages.warning (Printf.sprintf "Writing XHTML took %.3f secs" (time3 -. time2));
	Lwt.return
		(html
			(head ~a:[a_profile (uri_of_string "http://www.w3.org/2005/11/profile")]
				(title (pcdata "The Lambtex Manual"))
				[
				meta ~a: [a_http_equiv "content-type"] ~content: "text/html; charset=utf-8" ();
				Eliom_predefmod.Xhtml.css_link ~a:[(a_media [`All]); (a_title "Default")] ~uri:css_uri ()
				])
			(body [xhtml]))

let main_service =
	Eliom_predefmod.Xhtml.register_new_service 
		~path: [""]
		~get_params: Eliom_parameters.unit
		main_handler

