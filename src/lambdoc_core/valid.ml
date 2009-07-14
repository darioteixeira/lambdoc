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

let make_manuscript content bibs notes toc labelmap =
	{
	content = Block.get_frag content;
	bibs = bibs;
	notes = notes;
	toc = toc;
	labelmap = labelmap;
	}

let make_composition content =
	{
	content = Block.get_frag content;
	bibs = [];
	notes = [];
	toc = [];
	labelmap = Labelmap.create ();
	}


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
	let buf = Bin_prot.Utils.bin_dump ~header:true bin_writer_manuscript_t doc
	in buf

let serialize_composition_to_binprot doc =
	let buf = Bin_prot.Utils.bin_dump ~header:true bin_writer_composition_t doc
	in buf

let deserialize_manuscript_from_binprot buf =
	let r = ref 0
	in bin_read_manuscript_t buf r

let deserialize_composition_from_binprot buf =
	let r = ref 0
	in bin_read_composition_t buf r

