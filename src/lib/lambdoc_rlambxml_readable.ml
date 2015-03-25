(********************************************************************************)
(*	Lambdoc_rlambxml_reader.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license xmlt.
*)
(********************************************************************************)

module Parser = Lambdoc_rlambxml_parser


(********************************************************************************)
(**	{2 Exceptions}								*)
(********************************************************************************)

exception Reading_error of int * string


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

let ast_from_string =
	let where_rex = Pcre.regexp "^(?<before>.*)at line (?<line>\\d+), position (?<pos>\\d+)(?<after>.*)$" in
	fun ~linenum_offset ~inline_extdefs ~block_extdefs str ->
		try
			`Okay (Parser.parse ~linenum_offset ~inline_extdefs ~block_extdefs str)
		with exc ->
			let (line, msg) = match exc with
				| Pxp_types.At (where, exc) ->
					let subs = Pcre.exec ~rex:where_rex where in
					let line = int_of_string (Pcre.get_named_substring where_rex "line" subs) in
					let why = Pxp_types.string_of_exn exc in
					let new_why =
						try
							(* PXP does not provide raw access to line numbers, so we must parse the exception output. *)
							let subs = Pcre.exec ~rex:where_rex why in
							let line = int_of_string (Pcre.get_named_substring where_rex "line" subs) in
							let before = Pcre.get_named_substring where_rex "before" subs in
							let after = Pcre.get_named_substring where_rex "after" subs in
							before ^ "at line " ^ (string_of_int (line - 1)) ^ after
						with Not_found -> why
					in (Some (line - 1), new_why)
				| exc ->
					raise exc
			in `Error [(line, msg)]

