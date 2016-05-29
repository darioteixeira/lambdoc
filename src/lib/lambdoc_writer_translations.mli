(********************************************************************************)
(*  Lambdoc_writer_translations.mli
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
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
    description: string;
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
    description:string ->
    equation:Lambdoc_core.Inline.seq ->
    printout:Lambdoc_core.Inline.seq ->
    table:Lambdoc_core.Inline.seq ->
    figure:Lambdoc_core.Inline.seq ->
    part:Lambdoc_core.Inline.seq ->
    appendix:Lambdoc_core.Inline.seq ->
    section:Lambdoc_core.Inline.seq ->
    bibliography:Lambdoc_core.Inline.seq ->
    notes:Lambdoc_core.Inline.seq ->
    toc:Lambdoc_core.Inline.seq ->
    abstract:Lambdoc_core.Inline.seq ->
    paragraph:string ->
    t

val english_names: t
val french_names: t
val portuguese_names: t
val default: t

