(********************************************************************************)
(*  Test_lambdoc.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Tyxml
open Lambdoc_prelude
open Lambdoc_document


(********************************************************************************)
(** {1 Inner modules}                                                           *)
(********************************************************************************)

module Tyxml_backend = struct include Html module Svg = Svg end

module Html_writer = Lambdoc_whtml_writer.Make (Tyxml_backend)

module Filetype =
struct
    type source = Lambtex | Lambwiki | Lambxml | Markdown
    type target = Html | Sexp | Asexp
    type t = Source of source | Target of target

    let of_string = function
        | "tex"   -> Source Lambtex
        | "wiki"  -> Source Lambwiki
        | "xml"   -> Source Lambxml
        | "md"    -> Source Markdown
        | "html"  -> Target Html
        | "sexp"  -> Target Sexp
        | "asexp" -> Target Asexp
        | x       -> invalid_arg ("Filetype.of_string: " ^ x)

    let string_of_source = function
        | Lambtex  -> "tex"
        | Lambwiki -> "wiki"
        | Lambxml  -> "xml"
        | Markdown -> "md"

    let string_of_target = function
        | Html  -> "html"
        | Sexp  -> "sexp"
        | Asexp -> "asexp"

    let describe_source = function
        | Lambtex  -> "Lambtex"
        | Lambwiki -> "Lambwiki"
        | Lambxml  -> "Lambxml"
        | Markdown -> "Markdown"

    let describe_target = function
        | Html  -> "Html"
        | Sexp  -> "Sexp"
        | Asexp -> "Amnesiac Sexp"

    let to_string = function
        | Source s -> string_of_source s
        | Target t -> string_of_target t
end


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

let explain major minor case = match (major, minor) with
    | (_, "feature") ->
        begin match case with
            | "abstract"     -> "Abstract"
            | "bibliography" -> "Bibliography"
            | "boxout"       -> "Boxout environments"
            | "code"         -> "Inline highlighted source-code"
            | "entity"       -> "Entities"
            | "heredoc"      -> "Heredoc"
            | "image"        -> "Images"
            | "inline"       -> "Inline elements"
            | "list"         -> "Lists"
            | "macro"        -> "Macros"
            | "mathblk"      -> "Math blocks"
            | "mathinl"      -> "Math inline"
            | "notes"        -> "End notes"
            | "paragraph"    -> "Paragraphs"
            | "plain"        -> "Plain text"
            | "pullquote"    -> "Pull-quotes"
            | "qa"           -> "Q&A environments"
            | "quote"        -> "Quote environments"
            | "reference"    -> "Internal and external references"
            | "rule"         -> "Rules"
            | "sectioning"   -> "Sectioning"
            | "source"       -> "Source environments"
            | "subpage"      -> "Subpage environments"
            | "tabular"      -> "Tabular environments"
            | "theorem"      -> "Theorem environments"
            | "title"        -> "Titles"
            | "toc"          -> "Table of contents"
            | "unicode"      -> "Unicode text"
            | "verbatim"     -> "Verbatim environments"
            | "verse"        -> "Verse environments"
            | "wrapper"      -> "Wrapper environments"
            | _              -> assert false
        end
    | ("semantic", "error") ->
        begin match case with
            | "block_nesting"   -> "Nesting in block context"
            | "column"          -> "Column errors"
            | "counter"         -> "Counter errors"
            | "custom"          -> "Custom block errors"
            | "depth"           -> "Invalid macro/inline/block depth"
            | "empty"           -> "Empty elements"
            | "inline_nesting"  -> "Nesting in inline context"
            | "invalid_entity"  -> "Invalid entity"
            | "invalid_label"   -> "Invalid label parameter"
            | "invalid_order"   -> "Invalid order parameter"
            | "invalid_style"   -> "Invalid style parameter"
            | "level"           -> "Invalid level"
            | "macro"           -> "Macro errors"
            | "misplaced_label" -> "Misplaced label parameter"
            | "misplaced_order" -> "Misplaced order parameter"
            | "missing"         -> "Missing notes and bibliography"
            | "target"          -> "Target errors"
            | "wrapper"         -> "Wrapper errors"
            | _                 -> assert false
        end
    | ("lambxml", "error") ->
        begin match case with
            | "attribute" -> "Attributes"
            | "list"      -> "Lists"
            | "literal"   -> "Text-only content"
            | "malformed" -> "Malformed document"
            | "notempty"  -> "Non-empty elements"
            | "qa"        -> "Q&A environments"
            | _           -> assert false
        end
    | ("lambwiki", "error") ->
        begin match case with
            | "unterminated_literal" -> "Unterminated literal blocks"
            | "bad_literal_prefix"   -> "Mismatched prefix in literal blocks"
            | "misaligned_quotation" -> "Misaligned quotation blocks"
            | _                      -> assert false
        end
    | _ ->
        assert false

let dict =
    let dict = Hashtbl.create 10 in
    let alphas = Re.(rep1 (alt [rg 'a' 'z'; rg '0' '9'])) in
    let rex = Re.(compile (seq
        [
        bos;
        group (seq [alphas; char '_'; alphas]);                 (* First two words are the category *)
        char '_';
        group (seq [alphas; rep (seq [char '_'; alphas])]);     (* All other words before extension are the case *)
        char '.';
        group alphas;                                           (* Final word is the extension *)
        eos
        ])) in
    let process fname =
        try
            let groups = Re.exec rex fname in
            let category = Re.Group.get groups 1 in
            let case = Re.Group.get groups 2 in
            let ext = Filetype.of_string (Re.Group.get groups 3) in
            let subdict =
                try Hashtbl.find dict category
                with Not_found ->
                    let subdict = Hashtbl.create 20 in
                    Hashtbl.add dict category subdict;
                    subdict in
            let (sources, targets) = try Hashtbl.find subdict case with Not_found -> ([], []) in
            let variants = match ext with
                | Source source -> (source :: sources, targets)
                | Target target -> (sources, target :: targets) in
            Hashtbl.replace subdict case variants
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
            let chan = Pervasives.open_in (Printf.sprintf "%s.%s" fname (Filetype.to_string ftype)) in
            let contents = Pervasives.input_all chan in
            Pervasives.close_in chan;
            Hashtbl.replace cache ftype (fname, contents);
            contents

let string_of_xhtml xhtml =
    let open Html in
    let page =
        (html
            (head
                (title (pcdata "Lambdoc document"))
                [
                meta ~a:[a_charset "utf-8"] ();
                link ~a:[a_media [`All]; a_title "Default"] ~rel:[`Stylesheet] ~href:(uri_of_string "css/lambdoc.css") ()
                ])
            (body [xhtml])) in
    Format.asprintf "%a" (Html.pp ()) page

let execute fname source target () =
    let source_contents = read_file fname (Source source) in
    let target_contents = read_file fname (Target target) in
    let reader = match source with
        | Lambtex  -> Lambdoc_rlambtex_reader.Trivial.ambivalent_from_string ~options:()
        | Lambwiki -> Lambdoc_rlamblite_reader.Trivial.ambivalent_from_string ~options:`Lambwiki
        | Lambxml  -> Lambdoc_rlambxml_reader.Trivial.ambivalent_from_string ~options:()
        | Markdown -> Lambdoc_rlamblite_reader.Trivial.ambivalent_from_string ~options:`Markdown in
    let writer = match target with
        | Html  -> fun doc -> Html_writer.write_ambivalent doc |> string_of_xhtml
        | Sexp  -> Ambivalent.serialize
        | Asexp -> fun doc ->
            let doc' = match doc with
                | Valid doc ->
                    let foldmapper = Foldmap.Identity.amnesiac in
                    Ambivalent.Valid (foldmapper.valid foldmapper () doc |> snd)
                | Invalid _ ->
                    doc in
            Ambivalent.serialize doc' in
    let idiosyncrasies = Idiosyncrasies.make ~max_macro_depth:(Some 4) ~max_inline_depth:(Some 4) ~max_block_depth:(Some 4) () in
    let doc = reader ~idiosyncrasies source_contents in
    let str = writer doc ^ "\n" in
    Alcotest.(check string) fname str target_contents

let build_test major minor =
    let category = major ^ "_" ^ minor in
    let subdict = Hashtbl.find dict category in
    let foreach_case case (sources, targets) accum =
        let foreach_source source accum =
            let foreach_target target accum =
                let fname = category ^ "_" ^ case in
                let desc = Printf.sprintf "%s (%s -> %s)" (explain major minor case) (Filetype.describe_source source) (Filetype.describe_target target) in
                let test = (desc, `Quick, execute fname source target) in
                test :: accum in
            List.fold_right foreach_target targets accum in
        List.fold_right foreach_source sources accum in
    Hashtbl.fold foreach_case subdict []

let tests =
    [
    ("Lambtex features", build_test "lambtex" "feature");
    ("Lambwiki features", build_test "lambwiki" "feature");
    ("Lambxml features", build_test "lambxml" "feature");
    ("Markdown features", build_test "markdown" "feature");
    ("Lambwiki errors", build_test "lambwiki" "error");
    ("Lambxml errors", build_test "lambxml" "error");
    ("Semantic errors", build_test "semantic" "error");
    ]

let () =
    Alcotest.run "Lambdoc tests" tests

