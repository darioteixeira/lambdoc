open OptParse

type t =
	{
	debug: bool;
	category: Category.t;
	input_markup: Markup.input_t;
	output_markup: Markup.output_t;
	input_chan: Pervasives.in_channel;
	output_chan: Pervasives.out_channel;
	}

let debug_opt = StdOpt.store_true ()
let category_opt = Opt.value_option "CATEGORY" (Some `Manuscript) Category.of_string (fun exn str -> "Error in " ^ str)
let input_markup_opt = Opt.value_option "MARKUP" (Some `Lambtex) Markup.input_of_string (fun exn str -> "Error in " ^ str)
let output_markup_opt = Opt.value_option "MARKUP" (Some `Xhtml) Markup.output_of_string (fun exn str -> "Error in " ^ str)
let input_file_opt = StdOpt.str_option ~metavar:"FILE" ()
let output_file_opt = StdOpt.str_option ~metavar:"FILE" ()

let options = OptParser.make ()

let general = OptParser.add_group options "General options"
let markup = OptParser.add_group options "Definition of markup languages"
let file = OptParser.add_group options "Definition of I/O files"

let () =
	OptParser.add options ~group:general ~short_name:'d' ~long_name:"debug" ~help:"Show debug information" debug_opt;
	OptParser.add options ~group:markup ~short_name:'c' ~long_name:"category" ~help:"Document category(assume 'Manuscript' if not specified)" category_opt;
	OptParser.add options ~group:markup ~short_name:'f' ~long_name:"from" ~help:"Input markup (assume 'Lambtex' if not specified)" input_markup_opt;
	OptParser.add options ~group:markup ~short_name:'t' ~long_name:"to" ~help:"Output markup (assume 'Xhtml' if not specified)" output_markup_opt;
	OptParser.add options ~group:file ~short_name:'i' ~long_name:"in" ~help:"Input file name (will read from STDIN if not specified)" input_file_opt;
	OptParser.add options ~group:file ~short_name:'o' ~long_name:"out" ~help:"Output file name (will write to STDOUT if not specified)" output_file_opt


let parse () = match OptParser.parse_argv options with
	| hd::tl ->
		OptParser.usage options ();
		failwith "exit"
	| [] ->
		{
		debug = Opt.get debug_opt;
		category = Opt.get category_opt;
		input_markup = Opt.get input_markup_opt;
		output_markup = Opt.get output_markup_opt;
		input_chan = (match Opt.opt input_file_opt with Some name -> Pervasives.open_in name | None -> Pervasives.stdin);
		output_chan = (match Opt.opt output_file_opt with Some name -> Pervasives.open_out name | None -> Pervasives.stdout);
		}

