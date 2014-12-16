(********************************************************************************)
(*	Main.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Main interface to the Markdown reader.
	This module implements the {!Lambdoc_reader.Reader.READABLE} interface.
*)


(********************************************************************************)
(*	{1 Exceptions}								*)
(********************************************************************************)

exception Reading_error of int * string


(********************************************************************************)
(*	{1 Public functions and values}						*)
(********************************************************************************)

let ast_from_string ~linenum_offset ~inline_extdefs ~block_extdefs str =
	try Omd.of_string str |> Mapper.ast_of_omd
	with Mapper.Unsupported_feature x -> 
		let msg = Printf.sprintf "The document uses an unsupported feature (%s)" x in
		raise (Reading_error (linenum_offset, msg))

