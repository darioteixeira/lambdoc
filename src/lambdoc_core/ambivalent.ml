(********************************************************************************)
(*	Implementation file for Ambivalent module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

TYPE_CONV_PATH "Ambivalent"


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type 'a t =
	[ `Valid of 'a Valid.t
	| `Invalid of 'a Invalid.t
	] (*with sexp*)

type manuscript_t = [`Manuscript] t (*with sexp*)

type composition_t = [`Composition] t (*with sexp*)


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

let make_valid_manuscript content bibs notes toc labels =
	`Valid (Valid.make_manuscript content bibs notes toc labels)

let make_valid_composition content =
	`Valid (Valid.make_composition content)

let make_invalid_manuscript errors =
	`Invalid (Invalid.make_manuscript errors)

let make_invalid_composition errors =
	`Invalid (Invalid.make_composition errors)

(*
let serialize_manuscript doc =
	Sexplib.Sexp.to_string_mach (sexp_of_t Variety.sexp_of_t doc)

let serialize_composition =
	serialize_manuscript

let deserialize_manuscript str =
	t_of_sexp Variety.t_of_sexp (Sexplib.Sexp.of_string str)

let deserialize_composition =
	deserialize_manuscript
*)

