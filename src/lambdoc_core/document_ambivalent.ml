(********************************************************************************)
(*	Implementation file for Document_ambivalent.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	An ambivalent document is one that can either be valid or invalid.
	Document readers will always return a document of this type.
*)

TYPE_CONV_PATH "Document"

open Document_ref
open Document_block
open Document_ghost
open Document_settings
open Document_error
open Document_variety
open Document_valid
open Document_invalid


(********************************************************************************)
(**	{2 Ambivalent module}							*)
(********************************************************************************)

module Ambivalent:
sig
	type 'a t =
		[ `Valid of 'a Valid.t
		| `Invalid of 'a Invalid.t
		] (*with sexp*)

	type manuscript_t = [`Manuscript] t (*with sexp*)

	type composition_t = [`Composition] t (*with sexp*)

	val make_valid_manuscript:
		([< Block.super_block_t], [< `Composition | `Manuscript]) Block.t list ->
		Bib.t list ->
		Note.t list ->
		Block.heading_block_t list ->
		Label_dict.t ->
		manuscript_t

	val make_valid_composition:
		([< Block.super_block_t], [< `Composition]) Block.t list ->
		composition_t

	val make_invalid_manuscript:
		Error.t list ->
		manuscript_t

	val make_invalid_composition:
		Error.t list ->
		composition_t

	(*	
	val serialize_manuscript: manuscript_t -> string
	val serialize_composition: composition_t -> string
	val deserialize_manuscript: string -> manuscript_t
	val deserialize_composition: string -> composition_t
	*)
end =
struct
	type 'a t =
		[ `Valid of 'a Valid.t
		| `Invalid of 'a Invalid.t
		] (*with sexp*)

	type manuscript_t = [`Manuscript] t (*with sexp*)

	type composition_t = [`Composition] t (*with sexp*)

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
end

