(********************************************************************************)
(*	Implementation file for Valid module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

TYPE_CONV_PATH "Document"


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type valid_t =
	{
	content: Block.M.super_frag_t;
	bibs: Bib.t list;
	notes: Note.t list;
	toc: Block.M.heading_block_t list;
	labelmap: Labelmap.t;
	} (*with sexp*)

type 'a t = valid_t (*with sexp*)

type manuscript_t = [`Manuscript] t (*with sexp*)

type composition_t = [`Composition] t (*with sexp*)


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

let make_manuscript content bibs notes toc labelmap =
	{
	content = (content :> (Block.M.super_block_t, [ `Manuscript ]) Block.M.t list);
	bibs = bibs;
	notes = notes;
	toc = toc;
	labelmap = labelmap;
	}

let make_composition content =
	{
	content = (content :> (Block.M.super_block_t, [ `Composition ]) Block.M.t list);
	bibs = [];
	notes = [];
	toc = [];
	labelmap = Labelmap.create ();
	}

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

