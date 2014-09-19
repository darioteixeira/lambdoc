(********************************************************************************)
(*	Lambcmd.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Sexplib.Std
open Options
open Lambdoc_core

module String = BatString


(********************************************************************************)
(**	{1 Modules}								*)
(********************************************************************************)

module Tyxml_backend =
struct
	include Html5.M
	module Svg = Svg.M
end


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

let string_of_xhtml the_title xhtml =
	let open Html5.M in
	let page = (html
			(head
				(title (pcdata the_title))
				[
				meta ~a:[a_charset "utf-8"] ();
				link ~a:[a_media [`All]; a_title "Default"] ~rel:[`Stylesheet] ~href:(uri_of_string "css/lambdoc.css") ()
				])
			(body [xhtml])) in
	let buf = Buffer.create 1024 in
	Html5.P.print ~output:(Buffer.add_string buf) page;
	Buffer.contents buf


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
		| `Lambtex  -> Lambdoc_read_lambtex.Simple.ambivalent_from_string ~idiosyncrasies input_str
		| `Lambwiki -> Lambdoc_read_lambwiki.Simple.ambivalent_from_string ~idiosyncrasies input_str
		| `Lambxml  -> Lambdoc_read_lambxml.Simple.ambivalent_from_string ~idiosyncrasies input_str
		| `Markdown -> Lambdoc_read_markdown.Simple.ambivalent_from_string ~idiosyncrasies input_str
		| `Sexp     -> Lambdoc_core.Ambivalent.deserialize unit_of_sexp unit_of_sexp unit_of_sexp input_str in
	let output_str = match options.output_markup with
		| `Sexp  ->
			Lambdoc_core.Ambivalent.serialize sexp_of_unit sexp_of_unit sexp_of_unit doc
		| `Html5 ->
			let module Html5_writer = Lambdoc_write_html5.Make_simple (Tyxml_backend) in
			let valid_options = Html5_writer.({default_valid_options with translations = options.language}) in
			let xhtml = Html5_writer.write_ambivalent ~valid_options doc in
			string_of_xhtml options.title xhtml in
	output_string options.output_chan output_str;
	options.input_cleaner options.input_chan;
	options.output_cleaner options.output_chan;
	exit (match doc with Ambivalent.Valid _ -> 0 | Ambivalent.Invalid _ -> 3)

