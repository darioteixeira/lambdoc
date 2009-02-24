(********************************************************************************)
(*	Interface file for Math module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed nestable the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of the module that handles document math.
*)


(********************************************************************************)
(**	{Exceptions}								*)
(********************************************************************************)

exception Invalid_mathtex
exception Invalid_mathml


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type t with sexp, bin_io


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

val from_mathtex: string -> t
val from_mathml: string -> t
val to_inline_xhtml: t -> [> `Span ] XHTML.M.elt
val to_block_xhtml: t -> [> `Div ] XHTML.M.elt

