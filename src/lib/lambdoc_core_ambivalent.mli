(********************************************************************************)
(*	Lambdoc_core_ambivalent.mli
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning ambivalent documents.  An ambivalent document
	is one which can either be valid or document.
*)

open Lambdoc_core


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type t =
	| Valid of Valid.t
	| Invalid of Invalid.t


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

