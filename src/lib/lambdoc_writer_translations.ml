(********************************************************************************)
(*  Lambdoc_writer_translations.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core


(********************************************************************************)
(** {1 Type definitions}                                                        *)
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
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let make equation printout table figure part appendix section bibliography notes toc abstract paragraph =
    {equation; printout; table; figure; part; appendix; section; bibliography; notes; toc; abstract; paragraph}

let english_names = make
    [Inline.plain "Eq."]
    [Inline.plain "Print."]
    [Inline.plain "Tab."]
    [Inline.plain "Fig."]
    [Inline.plain "Part"]
    [Inline.plain "Appendix"]
    [Inline.plain "Section"]
    [Inline.plain "Bibliography"]
    [Inline.plain "Notes"]
    [Inline.plain "Table of Contents"]
    [Inline.plain "Abstract"]
    "Paragraph"

let french_names = make
    [Inline.plain "Eq."]
    [Inline.plain "List."]
    [Inline.plain "Tab."]
    [Inline.plain "Fig."]
    [Inline.plain "Partie"]
    [Inline.plain "Annexe"]
    [Inline.plain "Section"]
    [Inline.plain "Bibliographie"]
    [Inline.plain "Notes"]
    [Inline.plain "Index"]
    [Inline.plain "Résumé"]
    "Paragraphe"

let portuguese_names = make
    [Inline.plain "Eq."]
    [Inline.plain "List."]
    [Inline.plain "Tab."]
    [Inline.plain "Fig."]
    [Inline.plain "Parte"]
    [Inline.plain "Apêndice"]
    [Inline.plain "Secção"]
    [Inline.plain "Bibliografia"]
    [Inline.plain "Notas"]
    [Inline.plain "Índice"]
    [Inline.plain "Resumo"]
    "Parágrafo"

let default = english_names

