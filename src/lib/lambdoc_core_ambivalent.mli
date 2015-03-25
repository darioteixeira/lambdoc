(********************************************************************************)
(*	Lambdoc_core_ambivalent.mli
	Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning ambivalent documents.  An ambivalent document
	is one which can either be valid or document.
*)

module Bib = Lambdoc_core_bib
module Block = Lambdoc_core_block
module Error = Lambdoc_core_error
module Heading = Lambdoc_core_heading
module Invalid = Lambdoc_core_invalid
module Note = Lambdoc_core_note
module Valid = Lambdoc_core_valid


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type t =
	| Valid of Valid.t
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
	links:Valid.hdata_t ->
	images:Valid.hdata_t ->
	t

val make_invalid: Error.contextualized_t list -> t


(********************************************************************************)
(**	{2 Serialisation facilities}						*)
(********************************************************************************)

val serialize: t -> string
val deserialize: string -> t

