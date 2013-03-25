(********************************************************************************)
(*	Ambivalent.mli
	Copyright (c) 2009-2013 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning ambivalent documents.
*)

open Prelude
open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type t =
	| Valid of Valid.t
	| Invalid of Invalid.t
	with sexp


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Constructors}							*)
(********************************************************************************)

val make_valid:
	Block.frag_t ->
	Bib.t list ->
	Note.t list ->
	Heading.t list ->
	Alias.t list ->
	Valid.books_t ->
	Valid.labels_t ->
	Custom.dict_t ->
	t

val make_invalid: Error.t nelist -> t


(********************************************************************************)
(**	{2 Serialisation facilities}						*)
(********************************************************************************)

val serialize: t -> string
val deserialize: string -> t

