open Unix
open Lwt
open XHTML.M
open Lambdoc_proxy
open Client


let socket =
	{
	sockaddr = ADDR_INET (inet_addr_loopback, 9999);
	sockdomain = PF_INET;
	socktype = SOCK_STREAM;
	sockproto = 0;
	}


let test_handler sp () () =
	let chan = open_in "sample.ltex" in
	let inp = IO.input_channel chan in
	let src = IO.read_all inp in
	let () = IO.close_in inp in
	let () = close_in chan in
	let css_uri = Eliom_predefmod.Xhtml.make_uri (Eliom_services.static_dir sp) sp ["css"; "lambdoc.css"] in
	Client.ambivalent_manuscript_from_string ~socket ~markup:Markup.Lambtex src >>= fun doc ->
	let xhtml = Lambdoc_write_xhtml.Main.write_ambivalent_manuscript doc in
	Lwt.return
		(html
                        (head ~a:[a_profile (uri_of_string "http://www.w3.org/2005/11/profile")]
                                (title (pcdata "Lambtex Tutorial"))
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

