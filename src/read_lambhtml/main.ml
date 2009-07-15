(********************************************************************************)
(*	Main.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed nestable the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Main interface to the Lambhtml reader.
*)

open Lambdoc_reader


(********************************************************************************)
(*	{2 Reader module}							*)
(********************************************************************************)

module R : Reader.READER =
struct
	exception Reading_error of int * string

	let where_rex = Pcre.regexp "^In entity \\[toplevel\\] = PRIVATE, at line (?<line>\\d+), position \\d+:\\s*$"

	let ast_from_string str =
		try
			Parser.parse str
		with
			| Pxp_types.At (where, exc) ->
				let subs = Pcre.exec ~rex:where_rex where in
				let line = int_of_string (Pcre.get_named_substring where_rex "line" subs)
				in raise (Reading_error (line - 1, Pxp_types.string_of_exn exc))
			| Failure msg ->
				raise (Failure ("Internal parser error in " ^ msg))
end

module M = Reader.Make_reader (R)

include M

