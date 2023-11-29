(** Part 6 of the Lambdoc+Ocsigen tutorial.

    This instalment illustrates the relative simplicity of embedding markups
    within markups via the extension mechanism.  Check out the {!embed_markup}
    function to see the implementation.  Of note is the direct invocation of
    the [ast_from_string] function from each reader, the use [linenum_offset]
    so that error messages report the correct line number, and the mapping
    of each reader error into the generic {!Error.Reading_error} variant.
*)

open Eliom_content
open Html.F
open Lambdoc_prelude
open Lambdoc_document.Invalid
open Lambdoc_reader
open Extension.Trivial


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

let embed_markup tag ast_from_string =
    let f comm txt =
        match ast_from_string ~linenum_offset:Ast.(comm.comm_linenum-1) ~inline_extdefs:[] ~block_extdefs:[] txt with
            | `Okay ast   -> `Okay (ast, [])
            | `Error msgs -> `Error (List.map (fun (line, ident, msg) -> (line, ident, Error.Reading_error msg)) msgs)
    in (tag, Blkextcomm (Blkfun_lit f, [`Embeddable_blk]))

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
    let extcomms =
        [
        embed_markup "tex" (Lambdoc_rlambtex_readable.ast_from_string ~options:());
        embed_markup "wiki" (Lambdoc_rlamblite_readable.ast_from_string ~options:`Lambwiki);
        embed_markup "xml" (Lambdoc_rlambxml_readable.ast_from_string ~options:());
        embed_markup "md" (Lambdoc_rlamblite_readable.ast_from_string ~options:`Markdown);
        ] in
    let doc = Lambdoc_rlambtex_reader.Trivial.ambivalent_from_string ~extcomms source in
    let xdoc = Lambdoc_writer.write_ambivalent doc in
    let contents =
        [
        (xdoc : [ Html_types.div ] Html.F.elt :> [> Html_types.div ] Html.F.elt);
        p [a main_service [pcdata "Start again"] ()];
        ] in
    Lwt.return (make_page contents)

let () =
    Eliom_registration.Html.register main_service step1_handler

