(********************************************************************************)
(*	Valid.mli
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning valid documents.
*)

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
(**	{1 Public functions and values}						*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Constructors}							*)
(********************************************************************************)

val make:
	Block.frag_t ->
	Bib.t list ->
	Note.t list ->
	Heading.t list ->
	Alias.t list ->
	books_t ->
	labels_t ->
	Custom.dict_t ->
	t


(********************************************************************************)
(**	{2 Serialisation facilities}						*)
(********************************************************************************)

val serialize: t -> string
val deserialize: string -> t

