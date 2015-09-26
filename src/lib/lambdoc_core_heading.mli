(********************************************************************************)
(*  Lambdoc_core_heading.mli
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Definitions concerning heading elements.
*)

module Basic = Lambdoc_core_basic
module Inline = Lambdoc_core_inline
module Label = Lambdoc_core_label
module Level = Lambdoc_core_level
module Order = Lambdoc_core_order

open Basic


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

(** Ordering type for part headings.
*)
type part_order = (Order.ordinal, [ Order.ordinal Order.auto_given | Order.ordinal Order.user_given | Order.none_given ]) Order.t with sexp


(** Ordering type for section headings.
*)
type section_order = (Order.hierarchical, [Order.hierarchical Order.auto_given | Order.hierarchical Order.user_given | Order.none_given ]) Order.t with sexp


(** Part content.
*)
type part_content =
    | Custom_part of Inline.seq
    | Appendix
    with sexp


(** Section content.
*)
type section_content =
    | Custom_section of Inline.seq
    | Bibliography
    | Notes
    | Toc
    with sexp


(** Section locations.
*)
type section_location =
    | Mainbody
    | Appendixed
    with sexp


(** Heading blocks.
*)
type t =
    | Part of Label.t * part_order * part_content
    | Section of Label.t * section_order * section_location * Level.section * section_content
    with sexp


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val part: Label.t -> part_order -> Inline.seq -> t
val appendix: Label.t -> t
val section: Label.t -> section_order -> section_location -> Level.section -> Inline.seq -> t
val bibliography: Label.t -> t
val notes: Label.t -> t
val toc: Label.t -> t

