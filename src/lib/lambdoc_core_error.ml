(********************************************************************************)
(*  Lambdoc_core_error.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module Basic = Lambdoc_core_basic
module Blkcat = Lambdoc_core_blkcat
module Custom = Lambdoc_core_custom
module Level = Lambdoc_core_level
module Wrapper = Lambdoc_core_wrapper

open Sexplib.Std
open Basic


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type context =
    {
    error_line_number: int;
    error_line_before: string list;
    error_line_actual: string;
    error_line_after: string list;
    } [@@deriving sexp]


type invalid_parameter_reason =
    | Reason_is_empty_when_non_empty_mandatory
    | Reason_is_empty_when_forbidden
    | Reason_is_non_empty_when_forbidden of string
    | Reason_is_absent_when_mandatory
    [@@deriving sexp]


type target =
    | Target_bib
    | Target_note
    | Target_label
    [@@deriving sexp]


type msg =
    | Misplaced_label_parameter of invalid_parameter_reason
    | Misplaced_order_parameter of invalid_parameter_reason

    | Invalid_label of pointer
    | Invalid_order_format of string
    | Invalid_order_levels of string * Level.section * int

    | Invalid_style_bad_boolean of string * string
    | Invalid_style_bad_lang of string * string
    | Invalid_style_bad_numeric of string * string * int * int
    | Invalid_style_bad_colsfmt of string * string
    | Invalid_style_bad_cellfmt of string * string
    | Invalid_style_bad_classname of string
    | Invalid_style_bad_keyvalue of string
    | Invalid_style_misplaced_keyvalue of string * string
    | Invalid_style_misplaced_classname of string
    | Invalid_style_unknown_keyvalue of string * string

    | Invalid_entity_name of string
    | Invalid_entity_deci of string
    | Invalid_entity_hexa of string

    | Invalid_macro_nargs of pointer * string
    | Invalid_macro_argument_context
    | Invalid_macro_argument_number of string * int
    | Invalid_macro_call of pointer * int * int

    | Invalid_macro of pointer
    | Duplicate_macro of pointer
    | Undefined_macro of pointer

    | Excessive_macro_depth of int
    | Excessive_inline_depth of int
    | Excessive_block_depth of int

    | Invalid_custom of pointer
    | Duplicate_custom of pointer
    | Undefined_custom of pointer

    | Invalid_wrapper of Wrapper.kind

    | Invalid_section_level of int
    | Invalid_title_level of int

    | Invalid_counter of pointer
    | Mismatched_counter of pointer
    | Unexpected_counter of pointer

    | Invalid_mathtex of string
    | Invalid_mathml of string

    | Invalid_column_number of ident option * int * int * int
    | Mismatched_column_numbers of int list

    | Duplicate_target of pointer
    | Empty_target of pointer
    | Wrong_target of pointer * target * target
    | Undefined_target of pointer

    | Empty_source
    | Empty_verbatim
    | Empty_list
    | Empty_sequence
    | Empty_fragment

    | Unexpected_inline
    | Unexpected_block of Blkcat.t

    | Missing_bibliography
    | Missing_notes

    | Malformed_code_point
    | Reading_error of string
    | Unavailable_feature of string
    | Extension_error of string

    [@@deriving sexp]


type contextualized = context option * ident option * msg [@@deriving sexp]

type localized = int option * ident option * msg

type reading = int option * ident option * string

