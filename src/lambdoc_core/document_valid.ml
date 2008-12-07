(********************************************************************************)
(*	Implementation file for Document_valid.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of valid documents.
*)

TYPE_CONV_PATH "Document"

open Document_ref
open Document_block
open Document_ghost
open Document_settings
open Document_variety


(********************************************************************************)
(*	{2 Valid module}							*)
(********************************************************************************)

module Valid:
sig
	type valid_t =
		{
		content: Block.super_frag_t;
		bibs: Bib.t list;
		notes: Note.t list;
		toc: Block.heading_block_t list;
		labels: Label_dict.t;
		} (*with sexp*)

	type 'a t = valid_t (*with sexp*)

	type manuscript_t = [`Manuscript] t (*with sexp*)

	type composition_t = [`Composition] t (*with sexp*)

	val make_manuscript:
		([< Block.super_block_t], [< `Composition | `Manuscript]) Block.t list ->
		Bib.t list ->
		Note.t list ->
		Block.heading_block_t list ->
		Label_dict.t ->
		manuscript_t

	val make_composition: ([< Block.super_block_t], [< `Composition]) Block.t list -> composition_t

	(*
	val serialize_manuscript: manuscript_t -> string
	val serialize_composition: composition_t -> string
	val deserialize_manuscript: string -> manuscript_t
	val deserialize_composition: string -> composition_t
	*)
end =
struct
	type valid_t =
		{
		content: Block.super_frag_t;
		bibs: Bib.t list;
		notes: Note.t list;
		toc: Block.heading_block_t list;
		labels: Label_dict.t;
		} (*with sexp*)

	type 'a t = valid_t (*with sexp*)

	type manuscript_t = [`Manuscript] t (*with sexp*)

	type composition_t = [`Composition] t (*with sexp*)

	let make_manuscript content bibs notes toc labels =
		{
		content = (content : ('a, 'b) Block.t list :> (Block.super_block_t, 'b) Block.t list);
		bibs = bibs;
		notes = notes;
		toc = toc;
		labels = labels;
		}

	let make_composition content =
		{
		content = (content : ('a, 'b) Block.t list :> (Block.super_block_t, 'b) Block.t list);
		bibs = [];
		notes = [];
		toc = [];
		labels = Hashtbl.create 0;
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
end

