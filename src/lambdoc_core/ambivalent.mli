(********************************************************************************)
(*	Ambivalent.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning ambivalent documents.
*)

(********************************************************************************)
(**	{2 Type definitions}							*)
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
(**	{2 Public functions and values}						*)
(********************************************************************************)

val make_valid_manuscript:
	([< `Composition | `Manuscript ], _, _, _) Block.t list ->
	Bib.t list ->
	Note.t list ->
	Heading.t list ->
	Labelmap.t ->
	manuscript_t

val make_valid_composition:
	([< `Composition ], _, _, _) Block.t list ->
	composition_t

val make_invalid_manuscript: Error.t list -> manuscript_t

val make_invalid_composition: Error.t list -> composition_t


(********************************************************************************)
(**	{2 Serialisation facilities}						*)
(********************************************************************************)

(********************************************************************************)
(**	{3 Serialisation via Sexplib}						*)
(********************************************************************************)

val serialize_manuscript: manuscript_t -> string
val serialize_composition: composition_t -> string

val deserialize_manuscript: string -> manuscript_t
val deserialize_composition: string -> composition_t

