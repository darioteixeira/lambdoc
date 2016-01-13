(********************************************************************************)
(*  Lambdoc_core_target.mli
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Definitions concerning document targets.
*)

module Bib = Lambdoc_core_bib
module Custom = Lambdoc_core_custom
module Heading = Lambdoc_core_heading
module Note = Lambdoc_core_note
module Wrapper = Lambdoc_core_wrapper


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type visible_target =
    | Custom_target of Custom.key * Custom.kind * Custom.order
    | Wrapper_target of Wrapper.kind * Wrapper.order
    | Part_target of Heading.part_order
    | Section_target of Heading.section_location * Heading.section_order
    [@@deriving sexp]

type t =
    | Visible_target of visible_target
    | Bib_target of Bib.order
    | Note_target of Note.order
    [@@deriving sexp]


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val custom: Custom.key -> Custom.kind -> Custom.order -> t
val wrapper: Wrapper.kind -> Wrapper.order -> t
val part: Heading.part_order -> t
val section: Heading.section_location -> Heading.section_order -> t
val bib: Bib.order -> t
val note: Note.order -> t

