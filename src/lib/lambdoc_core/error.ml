(********************************************************************************)
(*	Error.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Sexplib.Std
open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type error_context_t =
	{
	error_line_number: int;
	error_line_before: string list;
	error_line_actual: string;
	error_line_after: string list;
	} with sexp


type invalid_parameter_reason_t =
	| Reason_is_empty_when_non_empty_mandatory
	| Reason_is_empty_when_forbidden
	| Reason_is_non_empty_when_forbidden of string
	| Reason_is_absent_when_mandatory
	with sexp


type target_t =
	| Target_bib
	| Target_note
	| Target_label
	with sexp


type error_msg_t =
	| Misplaced_label_parameter of Ident.t option * invalid_parameter_reason_t
	| Misplaced_order_parameter of Ident.t option * invalid_parameter_reason_t

	| Invalid_label of Ident.t option * Pointer.t
	| Invalid_order_format of Ident.t option * string
	| Invalid_order_levels of Ident.t option * string * Level.section_t * int

	| Invalid_style_bad_boolean of Ident.t option * string * string
	| Invalid_style_bad_lang of Ident.t option * string * string
	| Invalid_style_bad_numeric of Ident.t option * string * string * int * int
	| Invalid_style_bad_classname of Ident.t option * string
	| Invalid_style_bad_keyvalue of Ident.t option * string
	| Invalid_style_misplaced_keyvalue of Ident.t option * string * string
	| Invalid_style_misplaced_classname of Ident.t option * string
	| Invalid_style_unknown_keyvalue of Ident.t option * string * string
	| Invalid_style_extension of Ident.t option * string

	| Invalid_entity_name of string
	| Invalid_entity_deci of string
	| Invalid_entity_hexa of string

	| Invalid_macro_nargs of Pointer.t * string
	| Invalid_macro_argument_context
	| Invalid_macro_argument_number of string * int
	| Invalid_macro_call of Pointer.t * int * int

	| Invalid_macro of Ident.t option * Pointer.t
	| Duplicate_macro of Ident.t option * Pointer.t
	| Undefined_macro of Ident.t option * Pointer.t

	| Excessive_macro_depth of Pointer.t option * int
	| Excessive_inline_depth of Pointer.t option * int
	| Excessive_block_depth of Pointer.t option * int

	| Invalid_custom of Ident.t option * Pointer.t
	| Mismatched_custom of Ident.t option * Pointer.t * Custom.kind_t * Custom.kind_t
	| Duplicate_custom of Ident.t option * Pointer.t
	| Undefined_custom of Ident.t option * Pointer.t

	| Invalid_wrapper of Ident.t option * Wrapper.kind_t

	| Invalid_section_level of Ident.t option * int
	| Invalid_title_level of Ident.t option * int

	| Invalid_counter of Ident.t option * Pointer.t
	| Mismatched_counter of Ident.t option * Pointer.t
	| Unexpected_counter of Ident.t option * Pointer.t

	| Invalid_mathtex of Ident.t option * string
	| Invalid_mathml of Ident.t option * string
	| Invalid_column_number of Ident.t option * Ident.t option * int * int * int
	| Invalid_column_specifier of Ident.t option * string
	| Invalid_cell_specifier of Ident.t option * string

	| Duplicate_target of Ident.t option * Pointer.t
	| Empty_target of Ident.t option * Pointer.t
	| Wrong_target of Ident.t option * Pointer.t * target_t * target_t
	| Undefined_target of Ident.t option * Pointer.t

	| Empty_source of Ident.t option
	| Empty_verbatim of Ident.t option
	| Empty_list of Ident.t option
	| Empty_sequence of Ident.t option
	| Empty_fragment of Ident.t option
	| Unexpected_inline of Ident.t option
	| Unexpected_block of Ident.t option * Blkcat.t

	| Malformed_code_point
	| Reading_error of string
	| Unavailable_feature of Ident.t option * string

	| Unsupported_extension of Ident.t option
	| Failed_extension of Ident.t option * string

	with sexp


type t = error_context_t option * error_msg_t with sexp

