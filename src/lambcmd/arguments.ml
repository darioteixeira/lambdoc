(********************************************************************************)
(*  Arguments.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Cmdliner
open Lambdoc_prelude


(********************************************************************************)
(** {1 Private exceptions}                                                      *)
(********************************************************************************)

exception Cannot_guess_input_markup_from_stdin
exception Cannot_guess_input_markup_from_filename
exception Cannot_guess_output_markup_from_stdout
exception Cannot_guess_output_markup_from_filename


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type t =
    {
    title: string;
    language: Language.t;

    unrestricted: bool;
    max_macro_depth: int option;
    max_inline_depth: int option;
    max_block_depth: int option;

    input_markup: Markup.input;
    output_markup: Markup.output;

    input_chan: Pervasives.in_channel;
    output_chan: Pervasives.out_channel;
    input_cleaner: Pervasives.in_channel -> unit;
    output_cleaner: Pervasives.out_channel -> unit;
    }


(********************************************************************************)
(** {1 Private functions and values}                                            *)
(********************************************************************************)

(********************************************************************************)
(** {2 Declaration of manpage sections}                                         *)
(********************************************************************************)

let rendering = "OPTIONS: Document rendering"
let idiosyncrasies = "OPTIONS: Document idiosyncrasies"
let markup = "OPTIONS: Definition of markup languages"
let io = "OPTIONS: Definition of input/output files"


(********************************************************************************)
(** {2 Declaration of options}                                                  *)
(********************************************************************************)

let title =
    let doc = "Set the document title to $(docv)." in
    Arg.(value @@ opt string "Lambdoc document" @@ info ~docs:rendering ~docv:"TITLE" ~doc ["title"])

let language =
    let doc = "Language for I18N of document elements (either 'en', 'fr', or 'pt'). Assume 'en' if not specified." in
    Arg.(value @@ opt Language.converter Language.default @@ info ~docs:rendering ~docv:"LANG" ~doc ["l"; "lang"])

let unrestricted =
    let doc = "Unrestricted idiosyncrasies." in
    Arg.(value @@ flag @@ info ~docs:idiosyncrasies ~doc ["u"; "unrestricted"])

let max_macro_depth =
    let doc = "Maximum depth for macro calls." in
    Arg.(value @@ opt (some int) None @@ info ~docs:idiosyncrasies ~doc ["max-macro-depth"])

let max_inline_depth =
    let doc = "Maximum nesting depth for inline elements." in
    Arg.(value @@ opt (some int) None @@ info ~docs:idiosyncrasies ~doc ["max-inline-depth"])

let max_block_depth =
    let doc = "Maximum nesting depth for block elements." in
    Arg.(value @@ opt (some int) None @@ info ~docs:idiosyncrasies ~doc ["max-block-depth"])

let input_markup =
    let doc = "Input markup (either 'lambtex', 'lambxml', 'lambwiki', 'markdown', or 'sexp'). If not specified and argument $(i,IN_FILE) is provided, the input markup is guessed based on file extension." in
    Arg.(value @@ opt (some Markup.input_converter) None @@ info ~docs:markup ~docv:"IN_MARKUP" ~doc ["f"; "from"])

let output_markup =
    let doc = "Output markup (either 'xhtml' or 'sexp'). If not specified and argument $(i,OUT_FILE) is provided, the output markup is guessed based on file extension." in
    Arg.(value @@ opt (some Markup.output_converter) None @@ info ~docs:markup ~docv:"OUT_MARKUP" ~doc ["t"; "to"])

let input_file =
    let doc = "Input file name. If not specified, the document is read from STDIN, and argument $(i,IN_MARKUP) is mandatory." in
    Arg.(value @@ opt (some string) None @@ info ~docs:io ~docv:"IN_FILE" ~doc ["i"; "input"])

let output_file =
    let doc = "Output file name. If not specified, the document is written to STDOUT, and argument $(i,OUT_MARKUP) is mandatory." in
    Arg.(value @@ opt (some string) None @@ info ~docs:io ~docv:"OUT_FILE" ~doc ["o"; "output"])


(********************************************************************************)
(** {2 Functions for building Cmdliner terms and info}                          *)
(********************************************************************************)

let build title language unrestricted max_macro_depth max_inline_depth max_block_depth input_markup output_markup input_file output_file =
    let suffix name =
        let idx = String.rindex name '.' in
        String.sub name (idx + 1) (String.length name - idx - 1) in
    let (input_name, input_chan, input_cleaner) = match input_file with
        | Some name -> (Some name, Pervasives.open_in name, Pervasives.close_in)
        | None      -> (None, Pervasives.stdin, fun _ -> ()) in
    let (output_name, output_chan, output_cleaner) = match output_file with
        | Some name -> (Some name, Pervasives.open_out name, Pervasives.close_out)
        | None      -> (None, Pervasives.stdout, fun _ -> ()) in
    try
        let input_markup = match input_markup with
            | Some markup ->
                markup
            | None -> match input_file with
                | None ->
                    raise Cannot_guess_input_markup_from_stdin
                | Some name -> match Markup.input_parser (suffix name) with
                    | `Ok markup -> markup
                    | `Error _   -> raise Cannot_guess_input_markup_from_filename in
        let output_markup = match output_markup with
            | Some markup ->
                markup
            | None -> match output_file with
                | None ->
                    raise Cannot_guess_output_markup_from_stdout
                | Some name -> match Markup.output_parser (suffix name) with
                    | `Ok markup -> markup
                    | `Error _   -> raise Cannot_guess_output_markup_from_filename in
        `Ok
            {
            title; language;
            unrestricted; max_macro_depth; max_inline_depth; max_block_depth;
            input_markup; output_markup;
            input_chan; output_chan;
            input_cleaner; output_cleaner;
            }
    with
        | Cannot_guess_input_markup_from_stdin     -> `Error (true, "When using STDIN for input, you must explicitly specify input markup.")
        | Cannot_guess_input_markup_from_filename  -> `Error (true, "I'm unable to guess input markup based on filename. Please specify one.")
        | Cannot_guess_output_markup_from_stdout   -> `Error (true, "When using STDOUT for output, you must explicitly specify output markup")
        | Cannot_guess_output_markup_from_filename -> `Error (true, "I'm unable to guess output markup based on filename. Please specify one.")

let term = Term.(ret (const build $
    title $ language $
    unrestricted $ max_macro_depth $ max_inline_depth $ max_block_depth $
    input_markup $ output_markup $
    input_file $ output_file))

let info =
    let doc = "Command line utility for processing Lambdoc documents." in
    let man =
        [
        `S "BUGS";
        `P "Please report any issues at $(b,https://github.com/darioteixeira/lambdoc/issues)";
        `S "AUTHOR";
        `P "Dario Teixeira <dario.teixeira@nleyten.com>";
        ] in
    Term.info "lambcmd" ~version:"dev" ~doc ~man


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let parse () = match Term.eval (term, info) with
    | `Ok arg -> arg
    | _       -> exit 1

