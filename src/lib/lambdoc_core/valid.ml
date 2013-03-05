(********************************************************************************)
(*	Valid.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning valid documents.
*)

open Sexplib.Std
open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type books_t = (Book.isbn_t, Book.t) Hashtbl.t with sexp

type labels_t = (Label.t, Target.t) Hashtbl.t with sexp

type t =
	{
	content: Block.frag_t;
	bibs: Bib.t list;
	notes: Note.t list;
	toc: Heading.t list;
	images: Alias.t list;
	books: books_t;
	labels: labels_t;
	custom: Custom.dict_t;
	} with sexp


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Constructors}							*)
(********************************************************************************)

let make content bibs notes toc images books labels custom =
	{content; bibs; notes; toc; images; books; labels; custom}


(********************************************************************************)
(**	{2 Serialisation facilities}						*)
(********************************************************************************)

let serialize doc =
	Sexplib.Sexp.to_string_mach (sexp_of_t doc)

let deserialize str =
	t_of_sexp (Sexplib.Sexp.of_string str)

