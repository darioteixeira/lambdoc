(********************************************************************************)
(*  Lambdoc_writer_translations.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_document.Valid


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
    } [@@deriving make]


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let english_names = make
    ~description:"en"
    ~equation:[Inline.plain "Eq."]
    ~printout:[Inline.plain "Print."]
    ~table:[Inline.plain "Tab."]
    ~figure:[Inline.plain "Fig."]
    ~part:[Inline.plain "Part"]
    ~appendix:[Inline.plain "Appendix"]
    ~section:[Inline.plain "Section"]
    ~bibliography:[Inline.plain "Bibliography"]
    ~notes:[Inline.plain "Notes"]
    ~toc:[Inline.plain "Table of Contents"]
    ~abstract:[Inline.plain "Abstract"]
    ~paragraph:"Paragraph"

let french_names = make
    ~description:"fr"
    ~equation:[Inline.plain "Eq."]
    ~printout:[Inline.plain "List."]
    ~table:[Inline.plain "Tab."]
    ~figure:[Inline.plain "Fig."]
    ~part:[Inline.plain "Partie"]
    ~appendix:[Inline.plain "Annexe"]
    ~section:[Inline.plain "Section"]
    ~bibliography:[Inline.plain "Bibliographie"]
    ~notes:[Inline.plain "Notes"]
    ~toc:[Inline.plain "Index"]
    ~abstract:[Inline.plain "Résumé"]
    ~paragraph:"Paragraphe"

let portuguese_names = make
    ~description:"pt"
    ~equation:[Inline.plain "Eq."]
    ~printout:[Inline.plain "List."]
    ~table:[Inline.plain "Tab."]
    ~figure:[Inline.plain "Fig."]
    ~part:[Inline.plain "Parte"]
    ~appendix:[Inline.plain "Apêndice"]
    ~section:[Inline.plain "Secção"]
    ~bibliography:[Inline.plain "Bibliografia"]
    ~notes:[Inline.plain "Notas"]
    ~toc:[Inline.plain "Índice"]
    ~abstract:[Inline.plain "Resumo"]
    ~paragraph:"Parágrafo"

let default = english_names

