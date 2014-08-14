(********************************************************************************)
(*	Translations.mli
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of language-specific names for document elements.
*)

open Lambdoc_core


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type t =
	{
	equation: Inline.seq_t;
	printout: Inline.seq_t;
	table: Inline.seq_t;
	figure: Inline.seq_t;
	part: Inline.seq_t;
	appendix: Inline.seq_t;
	section: Inline.seq_t;
	bibliography: Inline.seq_t;
	notes: Inline.seq_t;
	toc: Inline.seq_t;
	abstract: Inline.seq_t;
	paragraph: string;
	}


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

val make:
	Lambdoc_core.Inline.seq_t ->
	Lambdoc_core.Inline.seq_t ->
	Lambdoc_core.Inline.seq_t ->
	Lambdoc_core.Inline.seq_t ->
	Lambdoc_core.Inline.seq_t ->
	Lambdoc_core.Inline.seq_t ->
	Lambdoc_core.Inline.seq_t ->
	Lambdoc_core.Inline.seq_t ->
	Lambdoc_core.Inline.seq_t ->
	Lambdoc_core.Inline.seq_t ->
	Lambdoc_core.Inline.seq_t ->
	string ->
	t

val english_names: t
val french_names: t
val portuguese_names: t
val default: t

