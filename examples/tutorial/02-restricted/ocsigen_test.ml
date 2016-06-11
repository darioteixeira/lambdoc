(********************************************************************************)
(*  Ocsigen_test.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Part 2 of the Lambdoc+Ocsigen tutorial.

    This example is almost identical to the previous one.  The sole difference
    is that a non-default idiosyncrasy is provided to the Lambtex reader: the
    document may not contain passages of bold text.
*)

open Eliom_content
open Html.F
open Lambdoc_prelude


(********************************************************************************)
(** {1 Modules}                                                                 *)
(********************************************************************************)

module Eliom_backend =
struct
    include Eliom_content.Html.F.Raw
    module Svg = Eliom_content.Svg.F.Raw
end

module Lambdoc_writer = Lambdoc_whtml_writer.Make (Eliom_backend)


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

let sample =
    let chan = Pervasives.open_in "sample.lambtex" in
    let sample = Pervasives.input_all chan in
    Pervasives.close_in chan;
    sample

let make_page content =
    let css_uri = make_uri (Eliom_service.static_dir ()) ["css"; "lambdoc.css"] in
    (html
        (head
            (title (pcdata "Lambdoc + Ocsigen : Basic example"))
            [css_link ~a:[(a_media [`All]); (a_title "Default")] ~uri:css_uri ()])
        (body content))

let main_service = Eliom_service.create
    ~meth:(Get Eliom_parameter.unit)
    ~id:(Path [])
    ()

let rec step1_handler () () =
    let step2_service = Eliom_registration.Html.create
        ~scope:Eliom_common.default_session_scope
        ~meth:(Post (Eliom_parameter.unit, Eliom_parameter.string "source"))
        ~id:(Fallback main_service)
        step2_handler in
    let step2_form e_source =
        [
        label [pcdata "Source:"];
        Form.textarea ~a:[a_rows 8; a_cols 80] ~name:e_source ~value:sample ();
        br ();
        Form.button_no_value ~button_type:`Submit [pcdata "Submit"];
        ] in
    Lwt.return (make_page [Form.post_form step2_service step2_form ()])

and step2_handler () source =
    let feature_ruleset = [(`Only `Feature_bold, `Deny)] in
    let idiosyncrasies = Lambdoc_document.Idiosyncrasies.make ~feature_ruleset () in
    let doc = Lambdoc_rlambtex_reader.Trivial.ambivalent_from_string ~idiosyncrasies source in
    let xdoc = Lambdoc_writer.write_ambivalent doc in
    let contents =
        [
        (xdoc : [ Html_types.div ] Html.F.elt :> [> Html_types.div ] Html.F.elt);
        p [a main_service [pcdata "Start again"] ()];
        ] in
    Lwt.return (make_page contents)

let () =
    Eliom_registration.Html.register main_service step1_handler

