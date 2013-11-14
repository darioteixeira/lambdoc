(********************************************************************************)
(*	Options.ml
	Copyright (c) 2009-2013 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open BatOptParse


(********************************************************************************)
(**	{1 Type definitions}							*)
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

	amazon_locale: Bookaml_amazon.Locale.t option;
	amazon_associate_tag: string option;
	amazon_access_key: string option;
	amazon_secret_key: string option;

	input_markup: Markup.input_t;
	output_markup: Markup.output_t;

	input_chan: Pervasives.in_channel;
	output_chan: Pervasives.out_channel;
	input_cleaner: Pervasives.in_channel -> unit;
	output_cleaner: Pervasives.out_channel -> unit;
	}


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

let debug_opt = StdOpt.store_true ()
let title_opt = StdOpt.str_option ~default:"Lambdoc document" ()
let language_opt = Opt.value_option "LANGUAGE" (Some Language.default) Language.of_string (fun exn str -> "Unknown language '" ^ str ^ "'")

let unrestricted_opt = StdOpt.store_true ()
let max_macro_depth_opt = StdOpt.int_option ()
let max_inline_depth_opt = StdOpt.int_option ()
let max_block_depth_opt = StdOpt.int_option ()

let amazon_locale_opt = Opt.value_option "LOCALE" None Bookaml_amazon.Locale.of_string (fun exn str -> "Unknown locale '" ^ str ^ "'")
let amazon_associate_tag_opt = StdOpt.str_option ()
let amazon_access_key_opt = StdOpt.str_option ()
let amazon_secret_key_opt = StdOpt.str_option ()

let input_markup_opt = Opt.value_option "MARKUP" (Some Markup.default_input) Markup.input_of_string (fun exn str -> "Unknown input markup '" ^ str ^ "'")
let output_markup_opt = Opt.value_option "MARKUP" (Some Markup.default_output) Markup.output_of_string (fun exn str -> "Unknown output markup '" ^ str ^ "'")

let input_file_opt = StdOpt.str_option ~metavar:"FILE" ()
let output_file_opt = StdOpt.str_option ~metavar:"FILE" ()

let options = OptParser.make ()

let general = OptParser.add_group options "General options"
let idiosyncrasies = OptParser.add_group options "Document idiosyncrasies"
let amazon = OptParser.add_group options "Amazon credentials (only required for book lookups)"
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

	OptParser.add options ~group:amazon ~long_name:"amazon-locale" ~help:"Amazon Web Services locale" amazon_locale_opt;
	OptParser.add options ~group:amazon ~long_name:"amazon-associate-tag" ~help:"Amazon Web Services associate tag" amazon_associate_tag_opt;
	OptParser.add options ~group:amazon ~long_name:"amazon-access_key" ~help:"Amazon Web Services access key" amazon_access_key_opt;
	OptParser.add options ~group:amazon ~long_name:"amazon-secret_key" ~help:"Amazon Web Services secret key" amazon_secret_key_opt;

	OptParser.add options ~group:markup ~short_name:'f' ~long_name:"from" ~help:"Input markup (either 'lambtex', 'lambxml', 'lambwiki', or 'sexp'; assume 'lambtex' if not specified)" input_markup_opt;
	OptParser.add options ~group:markup ~short_name:'t' ~long_name:"to" ~help:"Output markup (either 'xhtml' or 'sexp'; assume 'xhtml' if not specified)" output_markup_opt;

	OptParser.add options ~group:file ~short_name:'i' ~long_name:"in" ~help:"Input file name (read from STDIN if not specified)" input_file_opt;
	OptParser.add options ~group:file ~short_name:'o' ~long_name:"out" ~help:"Output file name (write to STDOUT if not specified)" output_file_opt


let parse () = match OptParser.parse_argv options with
	| hd::tl ->
		OptParser.usage options ();
		OptParser.error options "Error: invalid usage";
		raise Exit
	| [] ->
		let (input_chan, input_cleaner) = match Opt.opt input_file_opt with
			| Some name -> (Pervasives.open_in name, Pervasives.close_in)
			| None	    -> (Pervasives.stdin, fun _ -> ())
		and (output_chan, output_cleaner) = match Opt.opt output_file_opt with
			| Some name -> (Pervasives.open_out name, Pervasives.close_out)
			| None	    -> (Pervasives.stdout, fun _ -> ())
		in	{
			debug = Opt.get debug_opt;
			title = Opt.get title_opt;
			language = Opt.get language_opt;

			unrestricted = Opt.get unrestricted_opt;
			max_macro_depth = Opt.opt max_macro_depth_opt;
			max_inline_depth = Opt.opt max_inline_depth_opt;
			max_block_depth = Opt.opt max_block_depth_opt;

			amazon_locale = Opt.opt amazon_locale_opt;
			amazon_associate_tag = Opt.opt amazon_associate_tag_opt;
			amazon_access_key = Opt.opt amazon_access_key_opt;
			amazon_secret_key = Opt.opt amazon_secret_key_opt;

			input_markup = Opt.get input_markup_opt;
			output_markup = Opt.get output_markup_opt;

			input_chan = input_chan;
			output_chan = output_chan;
			input_cleaner = input_cleaner;
			output_cleaner = output_cleaner;
			}

