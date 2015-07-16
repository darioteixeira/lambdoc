(********************************************************************************)
(*  Ocsigen_test.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Part 7 of the Lambdoc+Ocsigen tutorial.

    This instalment depicts a common requirement for a web application:
    checking that the links in a document resolve successfully. Function
    {!pinger} below uses Cohttp to perform a HEAD request on every link,
    returning an error if the URL is malformed or if the request did not
    succeed.
*)

open Eliom_content
open Html5.F
open Lambdoc_reader


(********************************************************************************)
(** {1 Modules}                                                                 *)
(********************************************************************************)

module Lwt_monad = struct include Lwt let iter = Lwt_list.iter_p end

module Reader_extension = Lambdoc_reader.Extension.Make (Lwt_monad)

module Lambtex_reader = Lambdoc_rlambtex_reader.Make (Reader_extension)

module Eliom_backend =
struct
    include Eliom_content.Html5.F.Raw
    module Svg = Eliom_content.Svg.F.Raw
end

module Lambdoc_writer = Lambdoc_whtml5_writer.Make_trivial (Eliom_backend)


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

let pinger href =
    let open Cohttp in
    let open Cohttp_lwt_unix in
        try_lwt
            let uri = Uri.of_string href in
            lwt response = Client.head uri in
            response |> Response.status |> function
                | (#Code.client_error_status | #Code.server_error_status) as status ->
                    let code = Code.code_of_status status |> string_of_int in
                    let error = Lambdoc_core_error.Extension_error ("Error fetching link URL: " ^ code) in
                    Lwt.return (Some (`Error [error]))
                | _ ->
                    Lwt.return (Some (`Okay ""))
        with exc ->
            let error = Lambdoc_core_error.Extension_error ("Bad URL: " ^ Printexc.to_string exc) in
            Lwt.return (Some (`Error [error]))


let sample =
    let ch = Pervasives.open_in "sample.lambtex" in
    let sample = BatPervasives.input_all ch in
    Pervasives.close_in ch;
    sample


let make_page content =
    let css_uri = make_uri (Eliom_service.static_dir ()) ["css"; "lambdoc.css"] in
    (html
        (head
            (title (pcdata "Lambdoc + Ocsigen : Basic example"))
            [css_link ~a:[(a_media [`All]); (a_title "Default")] ~uri:css_uri ()])
        (body content))


let main_service = Eliom_service.Http.service 
    ~path:[""]
    ~get_params:Eliom_parameter.unit
    ()


let rec step1_handler () () =
    let step2_service = Eliom_registration.Html5.register_post_coservice
        ~scope:Eliom_common.default_session_scope
        ~fallback:main_service
        ~post_params:(Eliom_parameter.string "source")
        step2_handler in
    let step2_form e_source =
        [
        label ~a:[a_for e_source] [pcdata "Source:"];
        textarea ~a:[a_rows 8; a_cols 80] ~name:e_source ~value:sample ();
        br ();
        button ~button_type:`Submit [pcdata "Submit"];
        ] in
    Lwt.return (make_page [post_form step2_service step2_form ()])


and step2_handler () source =
    lwt doc = Lambtex_reader.ambivalent_from_string ~link_readers:[pinger] source in
    let xdoc = Lambdoc_writer.write_ambivalent doc in
    let contents =
        [
        (xdoc : [ Html5_types.div ] Html5.F.elt :> [> Html5_types.div ] Html5.F.elt);
        p [a main_service [pcdata "Start again"] ()];
        ] in
    Lwt.return (make_page contents)


let () =
    Eliom_registration.Html5.register main_service step1_handler

