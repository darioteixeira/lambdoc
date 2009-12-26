(********************************************************************************)
(*	Markup.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type input_t = [ `Lambtex | `Lamblite | `Lambhtml | `Sexp ]
type output_t = [ `Sexp | `Xhtml ]


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

val input_of_string: string -> input_t
val output_of_string: string -> output_t
val to_string: [< input_t | output_t ] -> string

