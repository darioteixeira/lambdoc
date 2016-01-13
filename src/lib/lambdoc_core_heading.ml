(********************************************************************************)
(*  Lambdoc_core_heading.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module Basic = Lambdoc_core_basic
module Inline = Lambdoc_core_inline
module Label = Lambdoc_core_label
module Level = Lambdoc_core_level
module Order = Lambdoc_core_order

open Basic


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type part_order = (Order.ordinal, [ Order.ordinal Order.auto_given | Order.ordinal Order.user_given | Order.none_given ]) Order.t [@@deriving sexp]

type section_order = (Order.hierarchical, [Order.hierarchical Order.auto_given | Order.hierarchical Order.user_given | Order.none_given ]) Order.t [@@deriving sexp]

type part_content =
    | Custom_part of Inline.seq
    | Appendix
    [@@deriving sexp]

type section_content =
    | Custom_section of Inline.seq
    | Bibliography
    | Notes
    | Toc
    [@@deriving sexp]

type section_location =
    | Mainbody
    | Appendixed
    [@@deriving sexp]

type t =
    | Part of Label.t * part_order * part_content
    | Section of Label.t * section_order * section_location * Level.section * section_content
    [@@deriving sexp]


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let part label order seq = Part (label, order, Custom_part seq)
let appendix label = Part (label, `None_given, Appendix)
let section label order location level seq = Section (label, order, location, level, Custom_section seq)
let bibliography label = Section (label, `None_given, Mainbody, Level.section 1, Bibliography)
let notes label = Section (label, `None_given, Mainbody, Level.section 1, Notes)
let toc label = Section (label, `None_given, Mainbody, Level.section 1, Toc)

