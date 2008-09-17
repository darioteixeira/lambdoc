(********************************************************************************)
(*	Definition of the module that handles document math.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed nestable the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

TYPE_CONV_PATH "Document"

open Document_basic


(********************************************************************************)
(*	{2 Helper functions}							*)
(********************************************************************************)

(**	Descends down the XML tree output by Blahtex, looking for a tag called
	"markup" which signals the beginning of the actual MathML data.
*)
let rec get_markup xml = match Xml.tag xml with
	| "markup" ->
		Xml.Element ("math", [], Xml.children xml)
	| _ ->
		let children = Xml.children xml
		in match children with
			| []    -> failwith "oops1"
			| [one] -> get_markup one
			| _     -> failwith "oops2"



(********************************************************************************)
(*	{2 Math module}								*)
(********************************************************************************)

module Math:
sig
	exception Invalid_mathtex
	exception Invalid_mathml

	type internal_t = mathml_t with sexp

	type t =
		| Mathtex of mathtex_t * internal_t
		| Mathml of mathml_t * internal_t
		with sexp

	val from_mathtex: mathtex_t -> t
	val from_mathml: mathml_t -> t
	val to_mathtex: t -> mathtex_t
	val to_mathml: t -> mathml_t
end =
struct
	exception Invalid_mathtex
	exception Invalid_mathml

	type internal_t = mathml_t with sexp

	type t =
		| Mathtex of mathtex_t * internal_t
		| Mathml of mathml_t * internal_t
		with sexp

	let from_mathtex txt =
		let (in_ch, out_ch) = Unix.open_process "/usr/local/bin/blahtex --mathml" in
		output_string out_ch txt;
		flush out_ch;
		close_out out_ch;
		let xml = Xml.parse_in in_ch in
		let markup = get_markup xml in
		let conversion = Xml.to_string markup in
		let _ = Unix.close_process (in_ch, out_ch)
		in Mathtex (txt, conversion)

	let from_mathml txt =
		Mathml (txt, "<math>" ^ txt ^ "</math>")

	let to_mathtex = function
		| Mathtex (mathtex, _)	-> mathtex
		| Mathml _		-> failwith "MathML -> TeX conversion not implemented yet"

	let to_mathml = function
		| Mathtex (_, internal)	-> internal
		| Mathml (_, internal)	-> internal
end

