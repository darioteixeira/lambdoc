(********************************************************************************)
(*	Lambdoc_core_error.mli
	Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of document errors.
*)

module Basic = Lambdoc_core_basic
module Blkcat = Lambdoc_core_blkcat
module Custom = Lambdoc_core_custom
module Wrapper = Lambdoc_core_wrapper

open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

(**	Definition of the error context.  The context includes any number
	of lines preceding the error, the error line proper, and a number
	of lines that follow.
*)
type context_t =
	{
	error_line_number: int;			(** Number of the line where the error occurred. *)
	error_line_before: string list;		(** Lines immediately before the error line. *)
	error_line_actual: string;		(** Contents of the line where the error is found. *)
	error_line_after: string list;		(** Lines immediately after the error line. *)
	} with sexp


(**	Reasons why a parameter can be invalid.
*)
type invalid_parameter_reason_t =
	| Reason_is_empty_when_non_empty_mandatory
	| Reason_is_empty_when_forbidden
	| Reason_is_non_empty_when_forbidden of string
	| Reason_is_absent_when_mandatory
	with sexp


(**	Expected targets.
*)
type target_t =
	| Target_bib
	| Target_note
	| Target_label
	with sexp


(**	The various types of error messages.
*)
type msg_t =
	| Misplaced_label_parameter of invalid_parameter_reason_t
	| Misplaced_order_parameter of invalid_parameter_reason_t

	| Invalid_label of Pointer.t
	| Invalid_order_format of string
	| Invalid_order_levels of string * Level.section_t * int

	| Invalid_style_bad_boolean of string * string
	| Invalid_style_bad_lang of string * string
	| Invalid_style_bad_numeric of string * string * int * int
	| Invalid_style_bad_classname of string
	| Invalid_style_bad_keyvalue of string
	| Invalid_style_misplaced_keyvalue of string * string
	| Invalid_style_misplaced_classname of string
	| Invalid_style_unknown_keyvalue of string * string

	| Invalid_entity_name of string
	| Invalid_entity_deci of string
	| Invalid_entity_hexa of string

	| Invalid_macro_nargs of Pointer.t * string
	| Invalid_macro_argument_context
	| Invalid_macro_argument_number of string * int
	| Invalid_macro_call of Pointer.t * int * int

	| Invalid_macro of Pointer.t
	| Duplicate_macro of Pointer.t
	| Undefined_macro of Pointer.t

	| Excessive_macro_depth of int
	| Excessive_inline_depth of int
	| Excessive_block_depth of int

	| Invalid_custom of Pointer.t
	| Mismatched_custom of Pointer.t * Custom.kind_t * Custom.kind_t
	| Duplicate_custom of Pointer.t
	| Undefined_custom of Pointer.t

	| Invalid_wrapper of Wrapper.kind_t

	| Invalid_section_level of int
	| Invalid_title_level of int

	| Invalid_counter of Pointer.t
	| Mismatched_counter of Pointer.t
	| Unexpected_counter of Pointer.t

	| Invalid_mathtex of string
	| Invalid_mathml of string
	| Invalid_column_number of Ident.t option * int * int * int
	| Invalid_column_specifier of string
	| Invalid_cell_specifier of string

	| Duplicate_target of Pointer.t
	| Empty_target of Pointer.t
	| Wrong_target of Pointer.t * target_t * target_t
	| Undefined_target of Pointer.t

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

	with sexp


(**	A contextualised error is a triple consisting of the context where
	the error occurred (where applicable), the offending command (also
	where applicable)  and the error message itself.
*)
type contextualized_t = context_t option * Ident.t option * msg_t with sexp

(**	A localized error is a triple consisting of the line number where
	the error occurred (where applicable), the offending command (also
	where applicable) and the error message itself.
*)
type localized_t = int option * Ident.t option * msg_t

(**	A reading error is a pair consisting of the line number where
	the error occurred (where applicable) and a string message.
*)
type reading_t = int option * string

