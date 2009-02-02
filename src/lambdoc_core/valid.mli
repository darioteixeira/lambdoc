(********************************************************************************)
(*	Interface file for Valid module.

	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning valid documents.
*)

(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type manuscript_t = private
	{
	content: Elem.frag_t;
	bibs: Elem.bib_t list;
	notes: Elem.note_t list;
	toc: Elem.heading_block_t list;
	labelmap: Labelmap.t;
	} (*with sexp*)


type composition_t = private Elem.frag_t (*with sexp*)


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

val make_manuscript:
	([< `Composition | `Manuscript ], _, _, _) Elem.block_t list ->
	Elem.bib_t list ->
	Elem.note_t list ->
	Elem.heading_block_t list ->
	Labelmap.t ->
	manuscript_t

val make_composition:
	([< `Composition ], _, _, _) Elem.block_t list ->
	composition_t


(********************************************************************************)
(**	{2 Serialisation facilities}						*)
(********************************************************************************)

(*
val serialize_manuscript: manuscript_t -> string
val serialize_composition: composition_t -> string
val deserialize_manuscript: string -> manuscript_t
val deserialize_composition: string -> composition_t
*)

