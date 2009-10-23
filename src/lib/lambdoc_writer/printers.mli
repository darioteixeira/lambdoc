(********************************************************************************)
(*	Printers.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Collection of predefined printers.
*)

open Lambdoc_core


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

val arabic: Order.ordinal_converter_t
val roman: Order.ordinal_converter_t
val mainbody: Order.hierarchical_converter_t
val appendixed: Order.hierarchical_converter_t

