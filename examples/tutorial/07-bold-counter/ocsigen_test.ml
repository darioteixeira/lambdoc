(********************************************************************************)
(*  Ocsigen_test.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Part 7 of the Lambdoc+Ocsigen tutorial.

    This instalment illustrates how the {!Lambdoc_core_foldmap} module can be used
    to perform deep traversals on documents.  Function {!count_bolds} below simply
    counts the number of bold inline sequences.  Note that it does not modify the
    document tree.
*)

open Eliom_content
open Html5.F
open Lambdoc_core
open Lambdoc_reader


(********************************************************************************)
(** {1 Modules}                                                                 *)
(********************************************************************************)

module Eliom_backend =
struct
    include Eliom_content.Html5.F.Raw
    module Svg = Eliom_content.Svg.F.Raw
end

module Lambdoc_writer = Lambdoc_whtml5_writer.Make (Eliom_backend)


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

let count_bolds doc =
    let module FM = Foldmap.Identity in
    let foldmapper =
        {
        FM.identity with
        bold = (fun fm acc attr seq -> FM.aux_seq Inline.bold fm (acc+1) attr seq);
        }
    in foldmapper.valid foldmapper 0 doc |> fst


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
        label [pcdata "Source:"];
        Form.textarea ~a:[a_rows 8; a_cols 80] ~name:e_source ~value:sample ();
        br ();
        Form.button_no_value ~button_type:`Submit [pcdata "Submit"];
        ] in
    Lwt.return (make_page [Form.post_form step2_service step2_form ()])


and step2_handler () source =
    let doc = Lambdoc_rlambtex_reader.Trivial.ambivalent_from_string source in
    let xdoc = Lambdoc_writer.write_ambivalent doc in
    let extra = match doc with
        | Invalid _ -> []
        | Valid doc -> [p [pcdata (Printf.sprintf "Document contains %d sequences of bold text" (count_bolds doc))]] in
    let contents =
        (
        (xdoc : [ Html5_types.div ] Html5.F.elt :> [> Html5_types.div ] Html5.F.elt) ::
        p [a main_service [pcdata "Start again"] ()] ::
        extra
        ) in
    Lwt.return (make_page contents)


let () =
    Eliom_registration.Html5.register main_service step1_handler

