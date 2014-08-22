(********************************************************************************)
(*	Main.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Main interface to the Lambxml reader.
*)

open Lambdoc_reader


(********************************************************************************)
(**	{1 Reader module}							*)
(********************************************************************************)

module Make = Reader.Make
(struct
	exception Reading_error of int * string

	(* PXP does not provide raw access to line numbers, so we must parse the exception output. *)
	let where_rex = Pcre.regexp "^(?<before>.*)at line (?<line>\\d+), position (?<pos>\\d+)(?<after>.*)$"

	let ast_from_string str =
		try
			Parser.parse str
		with
			| Pxp_types.At (where, exc) ->
				let subs = Pcre.exec ~rex:where_rex where in
				let line = int_of_string (Pcre.get_named_substring where_rex "line" subs) in
				let why = Pxp_types.string_of_exn exc in
				let new_why =
					try
						let subs = Pcre.exec ~rex:where_rex why in
						let line = int_of_string (Pcre.get_named_substring where_rex "line" subs) in
						let before = Pcre.get_named_substring where_rex "before" subs in
						let after = Pcre.get_named_substring where_rex "after" subs in
						before ^ "at line " ^ (string_of_int (line - 1)) ^ after
					with Not_found -> why
				in raise (Reading_error (line - 1, new_why))
			| Failure msg ->
				raise (Failure ("Internal parser error in " ^ msg))
end)

