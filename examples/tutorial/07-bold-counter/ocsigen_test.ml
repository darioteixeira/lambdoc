(** Part 7 of the Lambdoc+Ocsigen tutorial.

    This instalment illustrates how the {!Lambdoc_core_foldmap} module can be used
    to perform deep traversals on documents.  Function {!count_bolds} below simply
    counts the number of bold inline sequences.  Note that it does not modify the
    document tree.
*)

open Eliom_content
open Html.F
open Lambdoc_prelude
open Lambdoc_document
open Lambdoc_reader
open Valid


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
    let sample = Pervasives.input_all ch in
    Pervasives.close_in ch;
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
    let doc = Lambdoc_rlambtex_reader.Trivial.ambivalent_from_string source in
    let xdoc = Lambdoc_writer.write_ambivalent doc in
    let extra = match doc with
        | Invalid _ -> []
        | Valid doc -> [p [pcdata (Printf.sprintf "Document contains %d sequences of bold text" (count_bolds doc))]] in
    let contents =
        (
        (xdoc : [ Html_types.div ] Html.F.elt :> [> Html_types.div ] Html.F.elt) ::
        p [a main_service [pcdata "Start again"] ()] ::
        extra
        ) in
    Lwt.return (make_page contents)

let () =
    Eliom_registration.Html.register main_service step1_handler

