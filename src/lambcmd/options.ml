(********************************************************************************)
(*  Options.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open BatOptParse


(********************************************************************************)
(** {1 Private exceptions}                                                      *)
(********************************************************************************)

exception Leftover_options
exception Cannot_guess_input_markup_from_stdin
exception Cannot_guess_input_markup_from_filename
exception Cannot_guess_output_markup_from_stdout
exception Cannot_guess_output_markup_from_filename


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type t =
    {
    debug: bool;
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
(** {1 Functions and values}                                                    *)
(********************************************************************************)

let debug_opt = StdOpt.store_true ()
let title_opt = StdOpt.str_option ~default:"Lambdoc document" ()
let language_opt = Opt.value_option "LANGUAGE" (Some Language.default) Language.of_string (fun exn str -> "Unknown language '" ^ str ^ "'")

let unrestricted_opt = StdOpt.store_true ()
let max_macro_depth_opt = StdOpt.int_option ()
let max_inline_depth_opt = StdOpt.int_option ()
let max_block_depth_opt = StdOpt.int_option ()

let input_markup_opt = Opt.value_option "MARKUP" None Markup.input_of_string (fun exn str -> "Unknown input markup '" ^ str ^ "'")
let output_markup_opt = Opt.value_option "MARKUP" None Markup.output_of_string (fun exn str -> "Unknown output markup '" ^ str ^ "'")

let input_file_opt = StdOpt.str_option ~metavar:"FILE" ()
let output_file_opt = StdOpt.str_option ~metavar:"FILE" ()

let options = OptParser.make ()

let general = OptParser.add_group options "General options"
let idiosyncrasies = OptParser.add_group options "Document idiosyncrasies"
let markup = OptParser.add_group options "Definition of markup languages"
let file = OptParser.add_group options "Definition of I/O files"

let () =
    OptParser.add options ~group:general ~short_name:'v' ~long_name:"debug" ~help:"Show debug information" debug_opt;
    OptParser.add options ~group:general ~short_name:'e' ~long_name:"title" ~help:"Document title" title_opt;
    OptParser.add options ~group:general ~short_name:'l' ~long_name:"lang" ~help:"Language for I18N of document elements (either 'en', 'fr', or 'pt'; assume 'en' if not specified)" language_opt;

    OptParser.add options ~group:idiosyncrasies ~short_name:'u' ~long_name:"unrestricted" ~help:"Unrestricted idiosyncrasies" unrestricted_opt;
    OptParser.add options ~group:idiosyncrasies ~long_name:"max-macro-depth" ~help:"Maximum depth for macro calls" max_macro_depth_opt;
    OptParser.add options ~group:idiosyncrasies ~long_name:"max-inline-depth" ~help:"Maximum depth for inline elements" max_inline_depth_opt;
    OptParser.add options ~group:idiosyncrasies ~long_name:"max-block-depth" ~help:"Maximum depth for block elements" max_block_depth_opt;

    OptParser.add options ~group:markup ~short_name:'f' ~long_name:"from" ~help:"Input markup (either 'lambtex', 'lambxml', 'lambwiki', 'markdown', or 'sexp'; we try to guess based on file extension if not specified)" input_markup_opt;
    OptParser.add options ~group:markup ~short_name:'t' ~long_name:"to" ~help:"Output markup (either 'xhtml' or 'sexp'; we try to guess based on file extension if not specified)" output_markup_opt;

    OptParser.add options ~group:file ~short_name:'i' ~long_name:"in" ~help:"Input file name (read from STDIN if not specified)" input_file_opt;
    OptParser.add options ~group:file ~short_name:'o' ~long_name:"out" ~help:"Output file name (write to STDOUT if not specified)" output_file_opt


let parse () =
    let suffix name =
        let idx = String.rindex name '.' in
        String.sub name (idx + 1) (String.length name - idx - 1) in
    try
        begin match OptParser.parse_argv options with
            | hd::tl ->
                raise Leftover_options
            | [] ->
                let (input_name, input_chan, input_cleaner) = match Opt.opt input_file_opt with
                    | Some name -> (Some name, Pervasives.open_in name, Pervasives.close_in)
                    | None      -> (None, Pervasives.stdin, fun _ -> ())
                and (output_name, output_chan, output_cleaner) = match Opt.opt output_file_opt with
                    | Some name -> (Some name, Pervasives.open_out name, Pervasives.close_out)
                    | None      -> (None, Pervasives.stdout, fun _ -> ())
                in  {
                    debug = Opt.get debug_opt;
                    title = Opt.get title_opt;
                    language = Opt.get language_opt;

                    unrestricted = Opt.get unrestricted_opt;
                    max_macro_depth = Opt.opt max_macro_depth_opt;
                    max_inline_depth = Opt.opt max_inline_depth_opt;
                    max_block_depth = Opt.opt max_block_depth_opt;

                    input_markup =
                        begin match Opt.opt input_markup_opt with
                            | Some markup ->
                                markup
                            | None -> match input_name with
                                | None ->
                                    raise Cannot_guess_input_markup_from_stdin
                                | Some name ->
                                    try Markup.input_of_string (suffix name)
                                    with _ -> raise Cannot_guess_input_markup_from_filename
                        end;

                    output_markup =
                        begin match Opt.opt output_markup_opt with
                            | Some markup ->
                                markup
                            | None -> match output_name with
                                | None ->
                                    raise Cannot_guess_output_markup_from_stdout
                                | Some name ->
                                    try Markup.output_of_string (suffix name) with
                                    _ -> raise Cannot_guess_output_markup_from_filename
                        end;

                    input_chan = input_chan;
                    output_chan = output_chan;
                    input_cleaner = input_cleaner;
                    output_cleaner = output_cleaner;
                    }
        end
    with exc ->
        let msg = match exc with
            | Leftover_options                         -> "Invalid usage."
            | Cannot_guess_input_markup_from_stdin     -> "When using STDIN for input, you must explicitly specify input markup."
            | Cannot_guess_input_markup_from_filename  -> "I'm unable to guess input markup based on filename. Please specify one."
            | Cannot_guess_output_markup_from_stdout   -> "When using STDOUT for output, you must explicitly specify output markup"
            | Cannot_guess_output_markup_from_filename -> "I'm unable to guess output markup based on filename. Please specify one."
            | _                                        -> raise exc in
        OptParser.usage options ();
        OptParser.error options ("Error: " ^ msg);
        raise Exit

