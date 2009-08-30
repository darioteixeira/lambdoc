(********************************************************************************)
(*	Valid.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning valid documents.
*)

open Basic


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type 'a document_t =
	{
	content: Block.frag_t;
	bibs: Bib.t list;
	notes: Note.t list;
	toc: Heading.heading_t list;
	labelmap: Labelmap.t;
	bitmaps: alias_t list;
	} with sexp


type manuscript_t = [ `Manuscript ] document_t with sexp
type composition_t = [ `Composition ] document_t with sexp


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

val make_manuscript:
	([< `Composition | `Manuscript ], _, _, _, _) Block.t list ->
	Bib.t list ->
	Note.t list ->
	Heading.heading_t list ->
	Labelmap.t ->
	alias_t list ->
	manuscript_t

val make_composition:
	([< `Composition ], _, _, _, _) Block.t list ->
	alias_t list ->
	composition_t


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

