(********************************************************************************)
(*	Interface file for Ambivalent module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	An ambivalent document is one that can either be valid or invalid.
	Document readers will always return a document of this type.
*)

TYPE_CONV_PATH "Document"


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
(**	{2 Public values and functions}						*)
(********************************************************************************)

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

