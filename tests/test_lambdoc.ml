(********************************************************************************)
(*  Test_lambtex.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module Pervasives = BatPervasives

open Lambdoc_core

let execute markup fname () =
    let (ext, reader) = match markup with
        | `Lambtex -> ("tex", Lambdoc_rlambtex_reader.Trivial.ambivalent_from_string) in
    let cha1 = open_in (fname ^ ".sexp") in
    let cha2 = open_in (fname ^ "." ^ ext) in
    let sexp1 = Pervasives.input_all cha1 in
    let src2 = Pervasives.input_all cha2 in
    close_in cha1;
    close_in cha2;
    let sexp2 = reader src2 |> Ambivalent.serialize in
    Alcotest.(check string) fname sexp1 sexp2

let lambtex_set =
    [
    ("Plain text", `Quick, execute `Lambtex "lambtex_00_plain");
    ("Entities", `Quick, execute `Lambtex "lambtex_01_entity");
    ("Inline elements", `Quick, execute `Lambtex "lambtex_02_inline");
    ("Paragraphs", `Quick, execute `Lambtex "lambtex_03_paragraph");
    ("Lists", `Quick, execute `Lambtex "lambtex_04_list");
    ("Q&A environments", `Quick, execute `Lambtex "lambtex_05_qa");
    ("Verse environments", `Quick, execute `Lambtex "lambtex_06_verse");
    ("Quote environments", `Quick, execute `Lambtex "lambtex_07_quote");
    ("Mathematics", `Quick, execute `Lambtex "lambtex_08_math");
    ("Source environments", `Quick, execute `Lambtex "lambtex_09_source");
    ("Tabular environments", `Quick, execute `Lambtex "lambtex_10_tabular");
    ("Subpage environments", `Quick, execute `Lambtex "lambtex_11_subpage");
    ("Verbatim environments", `Quick, execute `Lambtex "lambtex_12_verbatim");
    ("Images", `Quick, execute `Lambtex "lambtex_13_image");
    ("Pull-quotes", `Quick, execute `Lambtex "lambtex_14_pullquote");
    ("Boxout environments", `Quick, execute `Lambtex "lambtex_15_boxout");
    ("Theorem environments", `Quick, execute `Lambtex "lambtex_16_theorem");
    ("Wrapper environments", `Quick, execute `Lambtex "lambtex_17_wrapper");
    ("Sectioning", `Quick, execute `Lambtex "lambtex_18_sectioning");
    ("Bibliography", `Quick, execute `Lambtex "lambtex_19_bibliography");
    ("End notes", `Quick, execute `Lambtex "lambtex_20_notes");
    ("Macros", `Quick, execute `Lambtex "lambtex_21_macro");
    ("Titles", `Quick, execute `Lambtex "lambtex_22_title");
    ("Abstract", `Quick, execute `Lambtex "lambtex_23_abstract");
    ("Table of contents", `Quick, execute `Lambtex "lambtex_24_toc");
    ]

let error_set =
    [
    ("Misplaced label parameter", `Quick, execute `Lambtex "error_00_misplaced_label");
    ("Misplaced order parameter", `Quick, execute `Lambtex "error_01_misplaced_order");
    ("Invalid label parameter", `Quick, execute `Lambtex "error_02_invalid_label");
    ("Invalid order parameter", `Quick, execute `Lambtex "error_03_invalid_order");
    ("Invalid style parameter", `Quick, execute `Lambtex "error_04_invalid_style");
    ]

let tests =
    [
    ("Parsing Lambtex", lambtex_set);
    ("Error messages", error_set);
    ]

let () =
    Alcotest.run "Lambdoc tests" tests

