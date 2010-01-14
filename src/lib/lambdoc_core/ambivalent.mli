(********************************************************************************)
(*	Ambivalent.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning ambivalent documents.
*)

open Prelude
open Basic


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

val make_valid_manuscript:
	([< `Composition | `Manuscript ], _, _, _, _) Block.t nelist ->
	Bib.t list ->
	Note.t list ->
	Heading.heading_t list ->
	Alias.t list ->
	Valid.labels_t ->
	Custom.dict_t ->
	manuscript_t

val make_valid_composition:
	([< `Composition ], _, _, _, _) Block.t nelist ->
	Alias.t list ->
	composition_t

val make_invalid_manuscript: Error.t nelist -> manuscript_t

val make_invalid_composition: Error.t nelist -> composition_t


(********************************************************************************)
(**	{2 Serialisation facilities}						*)
(********************************************************************************)

val serialize_manuscript: manuscript_t -> string
val serialize_composition: composition_t -> string

val deserialize_manuscript: string -> manuscript_t
val deserialize_composition: string -> composition_t

