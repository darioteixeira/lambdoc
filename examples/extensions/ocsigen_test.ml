(********************************************************************************)
(*  Ocsigen_test.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Eliom_content
open Html.F
open Lambdoc_prelude
open Lambdoc_reader


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
(** {1 Extensions}                                                              *)
(********************************************************************************)

open Extension.Trivial

let inlempty =
    let f comm = `Okay ([(comm, Ast.Plain "inlempty")], []) in
    ("inlempty", Inlextcomm (Inlfun_empty f))

let inlseq =
    let f comm seq = `Okay ([(comm, Ast.Bold seq)], []) in
    ("inlseq", Inlextcomm (Inlfun_seq f))

let inlraw =
    let f comm raw = `Okay ([(comm, Ast.Plain raw)], []) in
    ("inlraw", Inlextcomm (Inlfun_raw ("arg", f)))

let inlrawraw =
    let f comm raw1 raw2 = `Okay ([(comm, Ast.Plain (raw1 ^ raw2))], []) in
    ("inlrawraw", Inlextcomm (Inlfun_raw_raw ("arg1", "arg2", f)))

let inlrawseq =
    let f comm raw seq = `Okay ((comm, Ast.Plain raw) :: seq, []) in
    ("inlrawseq", Inlextcomm (Inlfun_raw_seq ("arg", f)))

let inlrawseqopt =
    let f comm raw mseq =
        let seq = match mseq with Some seq -> seq | None -> [(comm, Ast.Plain "seq")] in
        `Okay ((comm, Ast.Plain raw) :: seq, []) in
    ("inlrawseqopt", Inlextcomm (Inlfun_raw_seqopt ("arg", f)))

let blkempty =
    let f comm = `Okay ([comm, Ast.Verbatim "blkempty"], []) in
    ("blkempty", Blkextcomm (Blkfun_empty f, [`Embeddable_blk]))

let blkseq =
    let f comm seq = `Okay ([comm, Ast.Paragraph seq], []) in
    ("blkseq", Blkextcomm (Blkfun_seq f, [`Paragraph_blk; `Embeddable_blk]))

let blklit =
    let f comm raw = `Okay ([comm, Ast.Verbatim raw], []) in
    ("blklit", Blkextcomm (Blkfun_lit f, [`Embeddable_blk]))

let blkfrag =
    let f comm frag = `Okay ([comm, Ast.Quote frag], []) in
    ("blkfrag", Blkextcomm (Blkfun_frag f, [`Quotable_blk]))

let blkraw =
    let f comm raw = `Okay ([comm, Ast.Verbatim raw], []) in
    ("blkraw", Blkextcomm (Blkfun_raw ("arg", f), [`Embeddable_blk]))

let blkrawraw =
    let f comm raw1 raw2 = `Okay ([comm, Ast.Verbatim (raw1 ^ raw2)], []) in
    ("blkrawraw", Blkextcomm (Blkfun_raw_raw ("arg1", "arg2", f), [`Embeddable_blk]))

let extcomms =
    [
    inlempty;
    inlseq;
    inlraw;
    inlrawraw;
    inlrawseq;
    inlrawseqopt;
    blkempty;
    blkseq;
    blklit;
    blkfrag;
    blkraw;
    blkrawraw;
    ]


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

let lambtex_sample =
    let ch = Pervasives.open_in "sample.tex" in
    let sample = Pervasives.input_all ch in
    Pervasives.close_in ch;
    sample

let lambxml_sample =
    let ch = Pervasives.open_in "sample.xml" in
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
    let step2tex_service = Eliom_registration.Html.create
        ~scope:Eliom_common.default_session_scope
        ~meth:(Post (Eliom_parameter.unit, Eliom_parameter.string "source"))
        ~id:(Fallback main_service)
        (step2_handler Lambdoc_rlambtex_reader.Trivial.ambivalent_from_string) in
    let step2xml_service = Eliom_registration.Html.create
        ~scope:Eliom_common.default_session_scope
        ~meth:(Post (Eliom_parameter.unit, Eliom_parameter.string "source"))
        ~id:(Fallback main_service)
        (step2_handler Lambdoc_rlambxml_reader.Trivial.ambivalent_from_string) in
    let step2tex_form e_source =
        [
        label [pcdata "Lambtex source:"];
        Form.textarea ~a:[a_rows 8; a_cols 80] ~name:e_source ~value:lambtex_sample ();
        br ();
        Form.button_no_value ~button_type:`Submit [pcdata "Submit"];
        ] in
    let step2xml_form e_source =
        [
        label [pcdata "Lambxml source:"];
        Form.textarea ~a:[a_rows 8; a_cols 80] ~name:e_source ~value:lambxml_sample ();
        br ();
        Form.button_no_value ~button_type:`Submit [pcdata "Submit"];
        ] in
    let content =
        [
        Form.post_form step2tex_service step2tex_form ();
        Form.post_form step2xml_service step2xml_form ();
        ] in
    Lwt.return (make_page content)

and step2_handler ambivalent_from_string () source =
    let doc = ambivalent_from_string ~extcomms source in
    let xdoc = Lambdoc_writer.write_ambivalent doc in
    let contents =
        [
        (xdoc : [ Html_types.div ] Html.F.elt :> [> Html_types.div ] Html.F.elt);
        p [a main_service [pcdata "Start again"] ()];
        ] in
    Lwt.return (make_page contents)

let () =
    Eliom_registration.Html.register main_service step1_handler

