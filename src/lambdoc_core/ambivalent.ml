(********************************************************************************)
(*	Ambivalent.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

TYPE_CONV_PATH "Ambivalent"


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type manuscript_t =
	[ `Valid of Valid.manuscript_t
	| `Invalid of Invalid.manuscript_t
	] with sexp, bin_io


type composition_t =
	[ `Valid of Valid.composition_t
	| `Invalid of Invalid.composition_t
	] with sexp, bin_io


(********************************************************************************)
(**	{2 Functions and values}						*)
(********************************************************************************)

let make_valid_manuscript content bibs notes toc labels =
	`Valid (Valid.make_manuscript content bibs notes toc labels)

let make_valid_composition content =
	`Valid (Valid.make_composition content)

let make_invalid_manuscript errors =
	`Invalid (Invalid.make_manuscript errors)

let make_invalid_composition errors =
	`Invalid (Invalid.make_composition errors)


(********************************************************************************)
(**	{2 Serialisation facilities}						*)
(********************************************************************************)

(********************************************************************************)
(**	{3 Serialisation via Sexplib}						*)
(********************************************************************************)

let serialize_manuscript_to_sexp doc =
	Sexplib.Sexp.to_string_mach (sexp_of_manuscript_t doc)

let serialize_composition_to_sexp doc =
	Sexplib.Sexp.to_string_mach (sexp_of_composition_t doc)

let deserialize_manuscript_from_sexp str =
	manuscript_t_of_sexp (Sexplib.Sexp.of_string str)

let deserialize_composition_from_sexp str =
	composition_t_of_sexp (Sexplib.Sexp.of_string str)


(********************************************************************************)
(**	{3 Serialisation via Bin-prot}						*)
(********************************************************************************)

let serialize_manuscript_to_binprot doc =
	let buf = Bin_prot.Utils.bin_dump ~header:false bin_writer_manuscript_t doc
	in buf

let serialize_composition_to_binprot doc =
	let buf = Bin_prot.Utils.bin_dump ~header:false bin_writer_composition_t doc
	in buf

let deserialize_manuscript_from_binprot buf =
	let r = ref 0
	in bin_read_manuscript_t buf r

let deserialize_composition_from_binprot buf =
	let r = ref 0
	in bin_read_composition_t buf r

