(********************************************************************************)
(*	Lambdoc_rmarkdown_reader.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Main interface to the Markdown reader.
*)

module Mapper = Lambdoc_rmarkdown_mapper


(********************************************************************************)
(**	{1 Public modules}							*)
(********************************************************************************)

module Readable =
struct
	(************************************************************************)
	(*	{2 Exceptions}							*)
	(************************************************************************)

	exception Reading_error of int * string


	(************************************************************************)
	(*	{2 Public functions and values}					*)
	(************************************************************************)

	let ast_from_string ~linenum_offset ~inline_extdefs ~block_extdefs str =
		try
			`Okay (Omd.of_string str |> Mapper.ast_of_omd)
		with exc ->
			let msg = match exc with
				| Mapper.Unsupported_feature x ->
					Printf.sprintf "The document uses an unsupported feature (%s)" x
				| exc ->
					raise exc
			in `Error [(None, msg)]
end

module Make = Lambdoc_reader.Maker.Make (Readable)

module Trivial = Make (Lambdoc_reader.Extension.Trivial)

