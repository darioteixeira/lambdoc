(********************************************************************************)
(*	Dtd.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

let lambhtml_dtd =
	let config = {Pxp_types.default_config with Pxp_types.encoding = `Enc_utf8} in
	let source = Pxp_types.from_string (include_file "lambhtml.dtd")
	in Pxp_dtd_parser.parse_dtd_entity config source

