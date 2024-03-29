open Tyxml
open Lambdoc_prelude
open Lambdoc_document


(********************************************************************************)
(** {1 Modules}                                                                 *)
(********************************************************************************)

module Tyxml_backend =
struct
    include Html
    module Svg = Svg
end


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

let string_of_xhtml the_title xhtml =
    let open Html in
    let page =
        (html
            (head
                (title (pcdata the_title))
                [
                meta ~a:[a_charset "utf-8"] ();
                link ~a:[a_media [`All]; a_title "Default"] ~rel:[`Stylesheet] ~href:(uri_of_string "css/lambdoc.css") ()
                ])
            (body [xhtml])) in
    Format.asprintf "%a" (Html.pp ()) page

let () =
    let arguments = Arguments.parse () in
    let input_str = Pervasives.input_all arguments.input_chan in
    arguments.input_cleaner arguments.input_chan;
    let idiosyncrasies =
        let base =
            if arguments.unrestricted
            then Idiosyncrasies.unrestricted
            else Idiosyncrasies.default in
        {
        base with
            Idiosyncrasies.max_macro_depth = arguments.max_macro_depth;
            Idiosyncrasies.max_inline_depth = arguments.max_inline_depth;
            Idiosyncrasies.max_block_depth = arguments.max_block_depth;
        } in
    let doc = match arguments.input_markup with
        | `Lambtex  -> Lambdoc_rlambtex_reader.Trivial.ambivalent_from_string ~idiosyncrasies input_str
        | `Lambwiki -> Lambdoc_rlamblite_reader.Trivial.ambivalent_from_string ~options:`Lambwiki ~idiosyncrasies input_str
        | `Lambxml  -> Lambdoc_rlambxml_reader.Trivial.ambivalent_from_string ~idiosyncrasies input_str
        | `Markdown -> Lambdoc_rlamblite_reader.Trivial.ambivalent_from_string ~options:`Markdown ~idiosyncrasies input_str
        | `Sexp     -> Ambivalent.deserialize input_str in
    let output_str = match arguments.output_markup with
        | `Sexp  ->
            Ambivalent.serialize doc
        | `Html ->
            let module Html_writer = Lambdoc_whtml_writer.Make (Tyxml_backend) in
            let valid_options = Html_writer.({default_valid_options with translations = arguments.language}) in
            let xhtml = Html_writer.write_ambivalent ~valid_options doc in
            string_of_xhtml arguments.title xhtml in
    Pervasives.output_string arguments.output_chan output_str;
    Pervasives.output_char arguments.output_chan '\n';
    arguments.output_cleaner arguments.output_chan;
    exit (match doc with Ambivalent.Valid _ -> 0 | Ambivalent.Invalid _ -> 3)

