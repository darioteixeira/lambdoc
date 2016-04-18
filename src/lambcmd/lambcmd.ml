(********************************************************************************)
(*  Lambcmd.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Options
open Tyxml
open Lambdoc_core

module String = BatString


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
    let page = (html
            (head
                (title (pcdata the_title))
                [
                meta ~a:[a_charset "utf-8"] ();
                link ~a:[a_media [`All]; a_title "Default"] ~rel:[`Stylesheet] ~href:(uri_of_string "css/lambdoc.css") ()
                ])
            (body [xhtml])) in
    Format.asprintf "%a" (Html.pp ()) page


let () =
    let options = Options.parse () in
    let input_str = BatPervasives.input_all options.input_chan in
    let idiosyncrasies =
        let base =
            if options.unrestricted
            then Idiosyncrasies.unrestricted
            else Idiosyncrasies.default in
        {
        base with
            Idiosyncrasies.max_macro_depth = options.max_macro_depth;
            Idiosyncrasies.max_inline_depth = options.max_inline_depth;
            Idiosyncrasies.max_block_depth = options.max_block_depth;
        } in
    let doc = match options.input_markup with
        | `Lambtex  -> Lambdoc_rlambtex_reader.Trivial.ambivalent_from_string ~idiosyncrasies input_str
        | `Lambwiki -> Lambdoc_rlambwiki_reader.Trivial.ambivalent_from_string ~idiosyncrasies input_str
        | `Lambxml  -> Lambdoc_rlambxml_reader.Trivial.ambivalent_from_string ~idiosyncrasies input_str
        | `Markdown -> Lambdoc_rmarkdown_reader.Trivial.ambivalent_from_string ~idiosyncrasies input_str
        | `Sexp     -> Lambdoc_core.Ambivalent.deserialize input_str in
    let output_str = match options.output_markup with
        | `Sexp  ->
            Lambdoc_core.Ambivalent.serialize doc
        | `Html ->
            let module Html_writer = Lambdoc_whtml_writer.Make (Tyxml_backend) in
            let valid_options = Html_writer.({default_valid_options with translations = options.language}) in
            let xhtml = Html_writer.write_ambivalent ~valid_options doc in
            string_of_xhtml options.title xhtml in
    output_string options.output_chan output_str;
    output_char options.output_chan '\n';
    options.input_cleaner options.input_chan;
    options.output_cleaner options.output_chan;
    exit (match doc with Ambivalent.Valid _ -> 0 | Ambivalent.Invalid _ -> 3)

