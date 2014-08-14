(********************************************************************************)
(*	Ambivalent.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type t =
	| Valid of Valid.t
	| Invalid of Invalid.t
	with sexp


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Constructors}							*)
(********************************************************************************)

let make_valid content bibs notes toc images books labels custom =
	Valid (Valid.make content bibs notes toc images books labels custom)

let make_invalid errors =
	Invalid (Invalid.make errors)


(********************************************************************************)
(**	{2 Serialisation facilities}						*)
(********************************************************************************)

let serialize doc =
	Sexplib.Sexp.to_string_mach (sexp_of_t doc)

let deserialize str =
	t_of_sexp (Sexplib.Sexp.of_string str)

