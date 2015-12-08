(********************************************************************************)
(*  Test_lambdoc.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core


(********************************************************************************)
(** {1 Inner modules}                                                           *)
(********************************************************************************)

module Tyxml_backend = struct include Html5.M module Svg = Svg.M end

module Html5_writer = Lambdoc_whtml5_writer.Make (Tyxml_backend)

module Filetype =
struct
    type source = Lambtex | Lambxml
    type target = Html5 | Sexp
    type t = Source of source | Target of target

    let of_string = function
        | "tex"  -> Source Lambtex
        | "xml"  -> Source Lambxml
        | "html" -> Target Html5
        | "sexp" -> Target Sexp
        | x      -> invalid_arg ("Filetype.of_string: " ^ x)

    let string_of_source = function
        | Lambtex -> "tex"
        | Lambxml -> "xml"

    let string_of_target = function
        | Html5 -> "html"
        | Sexp  -> "sexp"

    let describe_source = function
        | Lambtex -> "Lambtex"
        | Lambxml -> "Lambxml"

    let describe_target = function
        | Html5 -> "Html5"
        | Sexp  -> "Sexp"

    let to_string = function
        | Source s -> string_of_source s
        | Target t -> string_of_target t
end



(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

let dict =
    let dict = Hashtbl.create 40 in
    let alphas = Re.(rep1 (alt [rg 'a' 'z'; rg '0' '9'; char '_'])) in
    let rex = Re.(compile (seq [bos; group alphas; char '.'; group alphas; eos])) in
    let process fname =
        try
            let groups = Re.exec rex fname in
            let fname = Re.get groups 1 in
            let ext = Filetype.of_string (Re.get groups 2) in
            let (sources, targets) = try Hashtbl.find dict fname with Not_found -> ([], []) in
            let v = match ext with
                | Source source -> (source :: sources, targets)
                | Target target -> (sources, target :: targets) in
            Hashtbl.replace dict fname v
        with
            | Not_found -> () in
    let hnd = Unix.opendir "." in
    begin
        try while true do process (Unix.readdir hnd) done
        with End_of_file -> Unix.closedir hnd
    end;
    dict


let read_file =
    let cache = Hashtbl.create 10 in
    fun fname ftype ->
        try
            let (fname', contents) = Hashtbl.find cache ftype in
            if fname = fname'
            then contents
            else raise Not_found
        with Not_found ->
            let chan = open_in (Printf.sprintf "%s.%s" fname (Filetype.to_string ftype)) in
            let contents = BatPervasives.input_all chan in
            close_in chan;
            Hashtbl.replace cache ftype (fname, contents);
            contents


let string_of_xhtml xhtml =
    let open Html5.M in
    let page = (html
            (head
                (title (pcdata "Lambdoc document"))
                [
                meta ~a:[a_charset "utf-8"] ();
                link ~a:[a_media [`All]; a_title "Default"] ~rel:[`Stylesheet] ~href:(uri_of_string "css/lambdoc.css") ()
                ])
            (body [xhtml])) in
    let buf = Buffer.create 1024 in
    Html5.P.print ~output:(Buffer.add_string buf) page;
    Buffer.contents buf


let execute fname source target () =
    let source_contents = read_file fname (Source source) in
    let target_contents = read_file fname (Target target) in
    let reader = match source with
        | Lambtex -> Lambdoc_rlambtex_reader.Trivial.ambivalent_from_string
        | Lambxml -> Lambdoc_rlambxml_reader.Trivial.ambivalent_from_string in
    let writer = match target with
        | Html5 -> fun doc -> Html5_writer.write_ambivalent doc |> string_of_xhtml
        | Sexp  -> Ambivalent.serialize in
    let doc = reader source_contents in
    let str = writer doc ^ "\n" in
    Alcotest.(check string) fname str target_contents


let build_test set =
    let foreach (base_desc, fname) accum =
        let (sources, targets) = Hashtbl.find dict fname in
        let foreach_source source accum =
            let foreach_target target accum =
                let desc = Printf.sprintf "%s (%s -> %s)" base_desc (Filetype.describe_source source) (Filetype.describe_target target) in
                let test = (desc, `Quick, execute fname source target) in
                test :: accum in
            List.fold_right foreach_target (List.sort Pervasives.compare targets) accum in
        List.fold_right foreach_source (List.sort Pervasives.compare sources) accum in
    List.fold_right foreach set []
        

let feature_set =
    [
    ("Plain text", "feature_00_plain");
    ("Entities", "feature_01_entity");
    ("Inline elements", "feature_02_inline");
    ("Paragraphs", "feature_03_paragraph");
    ("Lists", "feature_04_list");
    ("Q&A environments", "feature_05_qa");
    ("Verse environments", "feature_06_verse");
    ("Quote environments", "feature_07_quote");
    ("Mathematics", "feature_08_math");
    ("Source environments", "feature_09_source");
    ("Tabular environments", "feature_10_tabular");
    ("Subpage environments", "feature_11_subpage");
    ("Verbatim environments", "feature_12_verbatim");
    ("Images", "feature_13_image");
    ("Pull-quotes", "feature_14_pullquote");
    ("Boxout environments", "feature_15_boxout");
    ("Theorem environments", "feature_16_theorem");
    ("Wrapper environments", "feature_17_wrapper");
    ("Sectioning", "feature_18_sectioning");
    ("Bibliography", "feature_19_bibliography");
    ("End notes", "feature_20_notes");
    ("Macros", "feature_21_macro");
    ("Titles", "feature_22_title");
    ("Abstract", "feature_23_abstract");
    ("Table of contents", "feature_24_toc");
    ]

let error_set =
    [
    ("Misplaced label parameter", "error_00_misplaced_label");
    ("Misplaced order parameter", "error_01_misplaced_order");
    ("Invalid label parameter", "error_02_invalid_label");
    ("Invalid order parameter", "error_03_invalid_order");
    ("Invalid style parameter", "error_04_invalid_style");
    ("Invalid entity", "error_05_invalid_entity");
    ]

let tests =
    [
    ("Features", build_test feature_set);
    ("Error messages", build_test error_set);
    ]

let () =
    Alcotest.run "Lambdoc tests" tests

