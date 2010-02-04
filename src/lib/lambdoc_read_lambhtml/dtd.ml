(********************************************************************************)
(*	Dtd.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

let lambhtml_dtd =
	let config = {Pxp_types.default_config with Pxp_types.encoding = `Enc_utf8} in
	let source = Pxp_types.from_string (include_file "lambhtml.dtd") in
	let dtd = Pxp_dtd_parser.parse_dtd_entity config source in
	let add_entity name value =
		let value = "&#" ^ (string_of_int value) ^ ";" in
		let e = Pxp_dtd.Entity.create_internal_entity ~name ~value dtd
		in dtd#add_gen_entity e false in
	let () = Lambdoc_reader.Readconv.Basic_input.pxp_iter add_entity in
	let () = dtd#allow_arbitrary
	in dtd

