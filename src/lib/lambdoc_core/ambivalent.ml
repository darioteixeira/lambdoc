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

type ('a, 'b, 'c) t =
	| Valid of ('a, 'b, 'c) Valid.t
	| Invalid of Invalid.t
	with sexp


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Constructors}							*)
(********************************************************************************)

let make_valid ~content ~bibs ~notes ~toc ~labels ~customs ~links ~images ~externs =
	Valid (Valid.make ~content ~bibs ~notes ~toc ~labels ~customs ~links ~images ~externs)

let make_invalid errors =
	Invalid (Invalid.make errors)


(********************************************************************************)
(**	{2 Serialisation facilities}						*)
(********************************************************************************)

let serialize sexp_of_a sexp_of_b sexp_of_c doc =
	Sexplib.Sexp.to_string_mach (sexp_of_t sexp_of_a sexp_of_b sexp_of_c doc)

let deserialize a_of_sexp b_of_sexp c_of_sexp str =
	t_of_sexp a_of_sexp b_of_sexp c_of_sexp (Sexplib.Sexp.of_string str)

