(********************************************************************************)
(*	Ambivalent.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Sexplib.Conv


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type manuscript_t =
	[ `Valid of Valid.manuscript_t
	| `Invalid of Invalid.manuscript_t
	] with sexp


type composition_t =
	[ `Valid of Valid.composition_t
	| `Invalid of Invalid.composition_t
	] with sexp


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Constructors}							*)
(********************************************************************************)

let make_valid_manuscript content bibs notes toc images books labels custom =
	`Valid (Valid.make_manuscript content bibs notes toc images books labels custom)

let make_valid_composition content images books =
	`Valid (Valid.make_composition content images books)

let make_invalid_manuscript errors =
	`Invalid (Invalid.make_manuscript errors)

let make_invalid_composition errors =
	`Invalid (Invalid.make_composition errors)


(********************************************************************************)
(**	{2 Serialisation facilities}						*)
(********************************************************************************)

let serialize_manuscript doc =
	Sexplib.Sexp.to_string_mach (sexp_of_manuscript_t doc)

let serialize_composition doc =
	Sexplib.Sexp.to_string_mach (sexp_of_composition_t doc)

let deserialize_manuscript str =
	manuscript_t_of_sexp (Sexplib.Sexp.of_string str)

let deserialize_composition str =
	composition_t_of_sexp (Sexplib.Sexp.of_string str)

