(********************************************************************************)
(*	Ambivalent.mli
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning ambivalent documents.  An ambivalent document
	is one which can either be valid or document.
*)


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type ('a, 'b, 'c, 'd) t =
	| Valid of ('a, 'b, 'c, 'd) Valid.t
	| Invalid of Invalid.t
	with sexp


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Constructors}							*)
(********************************************************************************)

val make_valid:
	content:Block.frag_t ->
	bibs:Bib.t list ->
	notes:Note.t list ->
	toc:Heading.t list ->
	labels:Valid.labels_t ->
	customs:Valid.customs_t ->
	links:'a Valid.hdata_t ->
	images:'b Valid.hdata_t ->
	extinls:(Extcomm.extinl_t, 'c) Valid.xdata_t ->
	extblks:(Extcomm.extblk_t, 'd) Valid.xdata_t ->
	('a, 'b, 'c, 'd) t

val make_invalid: Error.t list -> ('a, 'b, 'c, 'd) t


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

