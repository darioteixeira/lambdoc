(********************************************************************************)
(*	Valid.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning valid documents.
*)

TYPE_CONV_PATH "Valid"

open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type 'a document_t =
	{
	content: Block.frag_t;
	bibs: Bib.t list;
	notes: Note.t list;
	toc: Heading.heading_t list;
	labelmap: Labelmap.t;
	images: Resource.t;
	} with sexp

type manuscript_t = [ `Manuscript ] document_t with sexp
type composition_t = [ `Composition ] document_t with sexp


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

let make_manuscript content bibs notes toc labelmap images =
	{
	content = Block.get_frag content;
	bibs = bibs;
	notes = notes;
	toc = toc;
	labelmap = labelmap;
	images = images;
	}

let make_composition content images =
	{
	content = Block.get_frag content;
	bibs = [];
	notes = [];
	toc = [];
	labelmap = Labelmap.create ();
	images = images;
	}


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

