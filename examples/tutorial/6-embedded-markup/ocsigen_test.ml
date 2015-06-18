(********************************************************************************)
(*  Ocsigen_test.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Part 6 of the Lambdoc+Ocsigen tutorial.

    This instalment illustrates the relative simplicity of embedding markups
    within markups via the extension mechanism.  Check out the {!embed_markup}
    function to see the implementation.  Of note is the direct invocation of
    the [ast_from_string] function from each reader, the use [linenum_offset]
    so that error messages report the correct line number, and the mapping
    of each reader error into the generic {!Error.Reading_error} variant.
*)

open Eliom_content
open Html5.F
open Lambdoc_core
open Lambdoc_reader
open Extension.Trivial


(********************************************************************************)
(** {1 Modules}                                                                 *)
(********************************************************************************)

module Eliom_backend =
struct
    include Eliom_content.Html5.F.Raw
    module Svg = Eliom_content.Svg.F.Raw
end

module Lambdoc_writer = Lambdoc_whtml5_writer.Make_trivial (Eliom_backend)


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

let embed_markup tag ast_from_string =
    let f comm txt =
        match ast_from_string ~linenum_offset:Ast.(comm.comm_linenum-1) ~inline_extdefs:[] ~block_extdefs:[] txt with
            | `Okay ast   -> `Okay (ast, [])
            | `Error msgs -> `Error (List.map (fun (line, msg) -> (line, None, Error.Reading_error msg)) msgs)
    in (tag, Blkextcomm (Blkfun_lit f, [`Embeddable_blk]))


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
    let extcomms =
        [
        embed_markup "tex" Lambdoc_rlambtex_readable.ast_from_string;
        embed_markup "wiki" Lambdoc_rlambwiki_readable.ast_from_string;
        embed_markup "xml" Lambdoc_rlambxml_readable.ast_from_string;
        embed_markup "md" Lambdoc_rmarkdown_readable.ast_from_string;
        ] in
    let doc = Lambdoc_rlambtex_reader.Trivial.ambivalent_from_string ~extcomms source in
    let xdoc = Lambdoc_writer.write_ambivalent doc in
    let contents =
        [
        (xdoc : [ Html5_types.div ] Html5.F.elt :> [> Html5_types.div ] Html5.F.elt);
        p [a main_service [pcdata "Start again"] ()];
        ] in
    Lwt.return (make_page contents)


let () =
    Eliom_registration.Html5.register main_service step1_handler

