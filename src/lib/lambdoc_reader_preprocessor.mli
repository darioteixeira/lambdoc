(********************************************************************************)
(*	Lambdoc_reader_preprocessor.mli
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Preprocessing on a document source.
*)


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

val verify_utf8: string -> [ `Okay | `Error of string * int list ]

