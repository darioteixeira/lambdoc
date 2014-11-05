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

type labels_t = (Label.t, Target.t) Hashtbl.t with sexp
type customs_t = (Custom.key_t, Inline.seq_t) Hashtbl.t with sexp
type 'a hdata_t = (Href.t, 'a) Hashtbl.t with sexp
type ('a, 'b) xdata_t = (Extkey.t, Ident.t * 'a * 'b) Hashtbl.t with sexp

type ('a, 'b, 'c, 'd) t =
	{
	content: Block.frag_t;
	bibs: Bib.t list;
	notes: Note.t list;
	toc: Heading.t list;
	labels: labels_t;
	customs: customs_t;
	links: 'a hdata_t;
	images: 'b hdata_t;
	extinls: (Extcomm.extinl_t, 'c) xdata_t;
	extblks: (Extcomm.extblk_t, 'd) xdata_t;
	} with sexp


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Constructors}							*)
(********************************************************************************)

val make:
	content:Block.frag_t ->
	bibs:Bib.t list ->
	notes:Note.t list ->
	toc:Heading.t list ->
	labels:labels_t ->
	customs:customs_t ->
	links:'a hdata_t ->
	images:'b hdata_t ->
	extinls:(Extcomm.extinl_t, 'c) xdata_t ->
	extblks:(Extcomm.extblk_t, 'd) xdata_t ->
	('a, 'b, 'c, 'd) t


(********************************************************************************)
(**	{2 Serialisation facilities}						*)
(********************************************************************************)

val serialize:
	('a -> Sexplib.Sexp.t) ->
	('b -> Sexplib.Sexp.t) ->
	('c -> Sexplib.Sexp.t) ->
	('d -> Sexplib.Sexp.t) ->
	('a, 'b, 'c, 'd) t ->
	string

val deserialize:
	(Sexplib.Sexp.t -> 'a) ->
	(Sexplib.Sexp.t -> 'b) ->
	(Sexplib.Sexp.t -> 'c) ->
	(Sexplib.Sexp.t -> 'd) ->
	string ->
	('a, 'b, 'c, 'd) t

val serialize_unitary: (unit, unit, unit, unit) t -> string

val deserialize_unitary: string -> (unit, unit, unit, unit) t

