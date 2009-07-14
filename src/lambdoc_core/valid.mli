(********************************************************************************)
(*	Valid.mli
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

type 'a document_t =
	{
	content: Block.frag_t;
	bibs: Bib.t list;
	notes: Note.t list;
	toc: Heading.t list;
	labelmap: Labelmap.t;
	} with sexp, bin_io


type manuscript_t = [ `Manuscript ] document_t with sexp, bin_io
type composition_t = [ `Composition ] document_t with sexp, bin_io


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

val make_manuscript:
	([< `Composition | `Manuscript ], _, _, _) Block.t list ->
	Bib.t list ->
	Note.t list ->
	Heading.t list ->
	Labelmap.t ->
	manuscript_t

val make_composition:
	([< `Composition ], _, _, _) Block.t list ->
	composition_t


(********************************************************************************)
(**	{2 Serialisation facilities}						*)
(********************************************************************************)

(********************************************************************************)
(**	{3 Serialisation via Sexplib}						*)
(********************************************************************************)

val serialize_manuscript_to_sexp: manuscript_t -> string
val serialize_composition_to_sexp: composition_t -> string

val deserialize_manuscript_from_sexp: string -> manuscript_t
val deserialize_composition_from_sexp: string -> composition_t


(********************************************************************************)
(**	{3 Serialisation via Bin-prot}						*)
(********************************************************************************)

val serialize_manuscript_to_binprot: manuscript_t -> Bin_prot.Common.buf
val serialize_composition_to_binprot: composition_t -> Bin_prot.Common.buf

val deserialize_manuscript_from_binprot: Bin_prot.Common.buf -> manuscript_t
val deserialize_composition_from_binprot: Bin_prot.Common.buf -> composition_t

