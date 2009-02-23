open XHTML.M
open Lambdoc_reader
open Features

(* --------------------------------------------------------------------	*)
(* Service "coucou".							*)
(* --------------------------------------------------------------------	*)

let coucou_handler sp () () =
	let src = IO.read_all (IO.input_channel (open_in "sample.ltex")) in
	let css_uri = Eliom_predefmod.Xhtml.make_uri (Eliom_services.static_dir sp) sp ["css"; "lambdoc.css"] in
	let accept_list = [ ] in
	let deny_list = [ ] in
	let default : default_t = `Accept in
	let time1 = Unix.gettimeofday () in
	let doc = Read_lambtex.Main.ambivalent_manuscript_from_string ~accept_list ~deny_list ~default src in
	let time2 = Unix.gettimeofday () in
	let pickle = Lambdoc_core.Ambivalent.serialize_manuscript doc in
	let time3 = Unix.gettimeofday () in
	let new_doc = Lambdoc_core.Ambivalent.deserialize_manuscript pickle in
	let time4 = Unix.gettimeofday () in
	let xhtml = Write_xhtml.Main.write_ambivalent_manuscript new_doc in
	let time5 = Unix.gettimeofday () in
	let () =
		Ocsigen_messages.warning (Printf.sprintf "Parsing took %.3f secs" (time2 -. time1));
		Ocsigen_messages.warning (Printf.sprintf "Serialising to S-exp took %.3f secs" (time3 -. time2));
		Ocsigen_messages.warning (Printf.sprintf "Deserialising from S-exp took %.3f secs" (time4 -. time3));
		Ocsigen_messages.warning (Printf.sprintf "Writing XHTML took %.3f secs" (time5 -. time4))
	in Lwt.return
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

