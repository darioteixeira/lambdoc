(********************************************************************************)
(*	Lambcmd.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open XHTML.M
open Options


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type processor_t =
	| Manuscript_io of (string -> Lambdoc_core.Ambivalent.manuscript_t) * (Lambdoc_core.Ambivalent.manuscript_t -> string)
	| Composition_io of (string -> Lambdoc_core.Ambivalent.composition_t) * (Lambdoc_core.Ambivalent.composition_t -> string)


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

let string_of_xhtml the_title xhtml =
	let page = (html
			(head ~a:[a_profile (uri_of_string "http://www.w3.org/2005/11/profile")]
				(title (pcdata the_title))
				[
				meta ~a:[a_http_equiv "content-type"] ~content:"text/html; charset=utf-8" ();
				link ~a:[a_href (uri_of_string "css/lambdoc.css"); a_rel [`Stylesheet]; a_media [`All]; a_title "Default"] ();
				])
			(body [xhtml]))
	in Xhtmlpretty.xhtml_print page


let get_processor options = match options.category with
	| `Manuscript ->
		let reader = match options.input_markup with
			| `Lambtex  -> (fun str -> Lambdoc_read_lambtex.Main.ambivalent_manuscript_from_string str)
			| `Lamblite -> (fun str -> Lambdoc_read_lamblite.Main.ambivalent_manuscript_from_string str)
			| `Lambhtml -> (fun str -> Lambdoc_read_lambhtml.Main.ambivalent_manuscript_from_string str)
			| `Sexp	    -> (fun str -> Lambdoc_core.Ambivalent.deserialize_manuscript str)
		and writer = match options.output_markup with
			| `Sexp  -> Lambdoc_core.Ambivalent.serialize_manuscript
			| `Xhtml -> (fun doc -> string_of_xhtml options.title (Lambdoc_write_xhtml.Main.write_ambivalent_manuscript doc))
		in Manuscript_io (reader, writer)
	| `Composition ->
		let reader = match options.input_markup with
			| `Lambtex  -> (fun str -> Lambdoc_read_lambtex.Main.ambivalent_composition_from_string str)
			| `Lamblite -> (fun str -> Lambdoc_read_lamblite.Main.ambivalent_composition_from_string str)
			| `Lambhtml -> (fun str -> Lambdoc_read_lambhtml.Main.ambivalent_composition_from_string str)
			| `Sexp	    -> (fun str -> Lambdoc_core.Ambivalent.deserialize_composition str)
		and writer = match options.output_markup with
			| `Sexp  -> Lambdoc_core.Ambivalent.serialize_composition
			| `Xhtml -> (fun doc -> string_of_xhtml options.title (Lambdoc_write_xhtml.Main.write_ambivalent_composition doc))
		in Composition_io (reader, writer)


let () =
	let options = Options.parse () in
	let input_str = Std.input_all options.input_chan in
	let processor = get_processor options in
	let (output_str, is_valid) = match processor with
		| Manuscript_io (reader, writer) ->
			let doc = reader input_str
			in (writer doc, match doc with `Valid _ -> true | _ -> false)
		| Composition_io (reader, writer) ->
			let doc = reader input_str
			in (writer doc, match doc with `Valid _ -> true | _ -> false)
	in
		output_string options.output_chan output_str;
		options.input_cleaner options.input_chan;
		options.output_cleaner options.output_chan;
		exit (if is_valid then 0 else 3)

