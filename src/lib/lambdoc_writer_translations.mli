(** Definition of language-specific names for document elements.
*)

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

val english_names: t
val french_names: t
val portuguese_names: t
val default: t

