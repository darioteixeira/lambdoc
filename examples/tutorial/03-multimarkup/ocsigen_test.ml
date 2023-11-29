(** Part 3 of the Lambdoc+Ocsigen tutorial.

    Again, this example is almost identical to the first one.  The only
    difference this time is that the user may choose one of the four markup
    languages currently supported by Lambdoc.  Note that this example uses
    the Litiom_choice module to simplify the creation of a select box (make
    sure the 'Litiom' package is installed on your system).
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

module Markup = Litiom_choice.Make
(struct
    type t = [ `Lambtex | `Lambwiki | `Lambxml | `Markdown ]

    let of_string = function
        | "lambtex"  -> `Lambtex
        | "lambwiki" -> `Lambwiki
        | "lambxml"  -> `Lambxml
        | "markdown" -> `Markdown
        | x      -> invalid_arg ("Markup.of_string: " ^ x)

    let to_string = function
        | `Lambtex  -> "lambtex"
        | `Lambwiki -> "lambwiki"
        | `Lambxml  -> "lambxml"
        | `Markdown -> "markdown"

    let describe = to_string

    let all = [ `Lambtex; `Lambwiki; `Lambxml; `Markdown ]
end)


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

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
        ~meth:(Post (Eliom_parameter.unit, Eliom_parameter.(Markup.param "markup" ** string "source")))
        ~id:(Fallback main_service)
        step2_handler in
    let step2_form (e_markup, e_source) =
        [
        label [pcdata "Markup:"];
        Markup.choose ~name:e_markup ~value:`Lambtex ();
        br ();
        label [pcdata "Source:"];
        Form.textarea ~a:[a_rows 8; a_cols 80] ~name:e_source ~value:sample ();
        br ();
        Form.button_no_value ~button_type:`Submit [pcdata "Submit"];
        ] in
    Lwt.return (make_page [Form.post_form step2_service step2_form ()])

and step2_handler () (markup, source) =
    let reader = match markup with
        | `Lambtex  -> Lambdoc_rlambtex_reader.Trivial.ambivalent_from_string ~options:()
        | `Lambwiki -> Lambdoc_rlamblite_reader.Trivial.ambivalent_from_string ~options:`Lambwiki
        | `Lambxml  -> Lambdoc_rlambxml_reader.Trivial.ambivalent_from_string ~options:()
        | `Markdown -> Lambdoc_rlamblite_reader.Trivial.ambivalent_from_string ~options:`Markdown in
    let doc = reader source in
    let xdoc = Lambdoc_writer.write_ambivalent doc in
    let contents =
        [
        (xdoc : [ Html_types.div ] Html.F.elt :> [> Html_types.div ] Html.F.elt);
        p [a main_service [pcdata "Start again"] ()];
        ] in
    Lwt.return (make_page contents)

let () =
    Eliom_registration.Html.register main_service step1_handler

