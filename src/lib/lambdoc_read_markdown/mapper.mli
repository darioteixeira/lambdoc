(********************************************************************************)
(*	Mapper.mli
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)


(********************************************************************************)
(*	{1 Exceptions}								*)
(********************************************************************************)

exception Unsupported_feature of string


(********************************************************************************)
(*	{1 Public functions and values}						*)
(********************************************************************************)

val ast_of_omd: Omd.t -> Lambdoc_reader.Ast.t

