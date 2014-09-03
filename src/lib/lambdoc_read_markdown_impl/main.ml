(********************************************************************************)
(*	Main.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Main interface to the Markdown reader.
*)

open Lambdoc_reader


(********************************************************************************)
(*	{2 Reader module}							*)
(********************************************************************************)

module Make = Reader.Make
(struct
	exception Reading_error of int * string

	let ast_from_string str =
		try Omd.of_string str |> Mapper.ast_of_omd
		with Mapper.Unsupported_feature x -> 
			let msg = Printf.sprintf "The document uses an unsupported feature (%s)" x in
			raise (Reading_error (0, msg))

end)

