(********************************************************************************)
(*  Lambdoc_writer_translations.mli
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Definition of language-specific names for document elements.
*)

open Lambdoc_core


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type t =
    {
    equation: Inline.seq;
    printout: Inline.seq;
    table: Inline.seq;
    figure: Inline.seq;
    part: Inline.seq;
    appendix: Inline.seq;
    section: Inline.seq;
    bibliography: Inline.seq;
    notes: Inline.seq;
    toc: Inline.seq;
    abstract: Inline.seq;
    paragraph: string;
    }


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val make:
    Lambdoc_core.Inline.seq ->
    Lambdoc_core.Inline.seq ->
    Lambdoc_core.Inline.seq ->
    Lambdoc_core.Inline.seq ->
    Lambdoc_core.Inline.seq ->
    Lambdoc_core.Inline.seq ->
    Lambdoc_core.Inline.seq ->
    Lambdoc_core.Inline.seq ->
    Lambdoc_core.Inline.seq ->
    Lambdoc_core.Inline.seq ->
    Lambdoc_core.Inline.seq ->
    string ->
    t

val english_names: t
val french_names: t
val portuguese_names: t
val default: t

