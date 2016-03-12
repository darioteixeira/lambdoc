(********************************************************************************)
(*  Test_lambdoc.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
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
    type source = Lambtex | Lambwiki | Lambxml | Markdown
    type target = Html5 | Sexp | Asexp
    type t = Source of source | Target of target

    let of_string = function
        | "tex"   -> Source Lambtex
        | "wiki"  -> Source Lambwiki
        | "xml"   -> Source Lambxml
        | "md"    -> Source Markdown
        | "html"  -> Target Html5
        | "sexp"  -> Target Sexp
        | "asexp" -> Target Asexp
        | x       -> invalid_arg ("Filetype.of_string: " ^ x)

    let string_of_source = function
        | Lambtex  -> "tex"
        | Lambwiki -> "wiki"
        | Lambxml  -> "xml"
        | Markdown -> "md"

    let string_of_target = function
        | Html5  -> "html"
        | Sexp   -> "sexp"
        | Asexp  -> "asexp"

    let describe_source = function
        | Lambtex  -> "Lambtex"
        | Lambwiki -> "Lambwiki"
        | Lambxml  -> "Lambxml"
        | Markdown -> "Markdown"

    let describe_target = function
        | Html5  -> "Html5"
        | Sexp   -> "Sexp"
        | Asexp  -> "Amnesiac Sexp"

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
        | Lambtex  -> Lambdoc_rlambtex_reader.Trivial.ambivalent_from_string
        | Lambwiki -> Lambdoc_rlambwiki_reader.Trivial.ambivalent_from_string
        | Lambxml  -> Lambdoc_rlambxml_reader.Trivial.ambivalent_from_string
        | Markdown -> Lambdoc_rmarkdown_reader.Trivial.ambivalent_from_string in
    let writer = match target with
        | Html5  -> fun doc -> Html5_writer.write_ambivalent doc |> string_of_xhtml
        | Sexp   -> Ambivalent.serialize
        | Asexp  -> fun doc ->
            let doc' = match doc with
                | Valid doc ->
                    let foldmapper = Foldmap.Identity.amnesiac in
                    foldmapper.valid foldmapper () doc |> snd |> Ambivalent.valid
                | Invalid _ ->
                    doc in
            Ambivalent.serialize doc' in
    let idiosyncrasies = Lambdoc_core_idiosyncrasies.make ~max_macro_depth:(Some 4) ~max_inline_depth:(Some 4) ~max_block_depth:(Some 4) () in
    let doc = reader ~idiosyncrasies source_contents in
    let str = writer doc ^ "\n" in
    Alcotest.(check string) fname str target_contents

let build_test ~prefix set =
    let foreach (base_desc, fname) accum =
        let fname = prefix ^ "_" ^ fname in
        let (sources, targets) = Hashtbl.find dict fname in
        let foreach_source source accum =
            let foreach_target target accum =
                let desc = Printf.sprintf "%s (%s -> %s)" base_desc (Filetype.describe_source source) (Filetype.describe_target target) in
                let test = (desc, `Quick, execute fname source target) in
                test :: accum in
            List.fold_right foreach_target (List.sort Pervasives.compare targets) accum in
        List.fold_right foreach_source (List.sort Pervasives.compare sources) accum in
    List.fold_right foreach set []

let common_feature_set1 =   (* Common to all markups *)
    [
    ("Plain text", "plain");
    ("Unicode text", "unicode");
    ("Inline elements", "inline");
    ("Paragraphs", "paragraph");
    ("Lists", "list");
    ("Quote environments", "quote");
    ("Source environments", "source");
    ("Sectioning", "sectioning");
    ]

let common_feature_set2 =   (* Common to Lambtex/Lambwiki/Lambxml *)
    [
    ("Entities", "entity");
    ("Verbatim environments", "verbatim");
    ]

let common_feature_set3 =   (* Common to Lambtex/Lambxml/Markdown *)
    [
    ("Images", "image");
    ("Rules", "rule");
    ]

let common_feature_set4 =   (* Common to Lambtex/Lambxml *)
    [
    ("Q&A environments", "qa");
    ("Verse environments", "verse");
    ("Mathematics", "math");
    ("Tabular environments", "tabular");
    ("Subpage environments", "subpage");
    ("Pull-quotes", "pullquote");
    ("Boxout environments", "boxout");
    ("Theorem environments", "theorem");
    ("Wrapper environments", "wrapper");
    ("Bibliography", "bibliography");
    ("End notes", "notes");
    ("Macros", "macro");
    ("Titles", "title");
    ("Abstract", "abstract");
    ("Table of contents", "toc");
    ("Internal and external references", "reference");
    ("Inline highlighted source-code", "code");
    ]

let lambtex_feature_set =
    [
    ("Heredoc", "heredoc");
    ]

let lambxml_error_set =
    [
    ("Attributes", "attribute");
    ("Lists", "list");
    ("Text-only content", "literal");
    ("Malformed document", "malformed");
    ("Non-empty elements", "notempty");
    ("Q&A environments", "qa");
    ]

let semantic_error_set =
    [
    ("Misplaced label parameter", "misplaced_label");
    ("Misplaced order parameter", "misplaced_order");
    ("Invalid label parameter", "invalid_label");
    ("Invalid order parameter", "invalid_order");
    ("Invalid style parameter", "invalid_style");
    ("Invalid entity", "invalid_entity");
    ("Macro errors", "macro");
    ("Invalid macro/inline/block depth", "depth");
    ("Custom block errors", "custom");
    ("Wrapper errors", "wrapper");
    ("Invalid level", "level");
    ("Counter errors", "counter");
    ("Column errors", "column");
    ("Target errors", "target");
    ("Empty elements", "empty");
    ("Nesting in inline context", "inline_nesting");
    ("Nesting in block context", "block_nesting");
    ("Missing notes and bibliography", "missing");
    ]

let tests =
    [
    ("Lambtex features", build_test ~prefix:"lambtex_feature"
        (common_feature_set1 @ common_feature_set2 @ common_feature_set3 @ common_feature_set4 @ lambtex_feature_set));
    ("Lambwiki features", build_test ~prefix:"lambwiki_feature"
        (common_feature_set1 @ common_feature_set2));
    ("Lambxml features", build_test ~prefix:"lambxml_feature"
        (common_feature_set1 @ common_feature_set2 @ common_feature_set3 @ common_feature_set4));
    ("Markdown features", build_test ~prefix:"markdown_feature"
        (common_feature_set1 @ common_feature_set3));
    ("Lambxml errors", build_test ~prefix:"lambxml_error"
        lambxml_error_set);
    ("Semantic errors", build_test ~prefix:"semantic_error"
        semantic_error_set);
    ]

let () =
    Alcotest.run "Lambdoc tests" tests

