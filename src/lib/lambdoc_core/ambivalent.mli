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

type ('a, 'b, 'c) t =
	| Valid of ('a, 'b, 'c) Valid.t
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
	links:'a Valid.payload_t ->
	images:'b Valid.payload_t ->
	externs:'c Valid.payload_t ->
	('a, 'b, 'c) t

val make_invalid: Error.t list -> ('a, 'b, 'c) t


(********************************************************************************)
(**	{2 Serialisation facilities}						*)
(********************************************************************************)

val serialize: ('a -> Sexplib.Sexp.t) -> ('b -> Sexplib.Sexp.t) -> ('c -> Sexplib.Sexp.t) -> ('a, 'b, 'c) t -> string
val deserialize: (Sexplib.Sexp.t -> 'a) -> (Sexplib.Sexp.t -> 'b) -> (Sexplib.Sexp.t -> 'c) -> string -> ('a, 'b, 'c) t

