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
	content: Block.frag_t;
	bibs: Block.bib_t list;
	notes: Block.note_t list;
	toc: Block.heading_block_t list;
	labelmap: Labelmap.t;
	} (*with sexp*)


type composition_t = private Block.frag_t (*with sexp*)


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

val make_manuscript:
	([< `Composition | `Manuscript ], _, _, _) Block.t list ->
	Block.bib_t list ->
	Block.note_t list ->
	Block.heading_block_t list ->
	Labelmap.t ->
	manuscript_t

val make_composition:
	([< `Composition ], _, _, _) Block.t list ->
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

