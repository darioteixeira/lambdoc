(********************************************************************************)
(*	Error.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of document errors.
*)

TYPE_CONV_PATH "Error"

open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

(**	Definition of the error context.  The context includes any number
	of lines preceding the error, the error line proper, and a number
	of lines that follow.
*)
type error_context_t =
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


(**	Block categories.
*)
type blk_category_t =
	[ `Any_blk
	| `Paragraph_blk
	| `Equation_blk
	| `Printout_blk
	| `Table_blk
	| `Figure_blk
	] with sexp


(**	The various types of error messages.
*)
type error_msg_t =
	| Bad_label_parameter of tag_t option * invalid_parameter_reason_t
	| Bad_order_parameter of tag_t option * invalid_parameter_reason_t
	| Bad_extra_parameter of tag_t option * invalid_parameter_reason_t

	| Nested_link of tag_t option
	| Empty_listing of tag_t option
	| Unexpected_block of tag_t option * blk_category_t

	| Invalid_extra_boolean_parameter of tag_t option * string * string
	| Invalid_extra_numeric_parameter of tag_t option * string * string
	| Invalid_extra_bullet_parameter of tag_t option * string * string
	| Invalid_extra_numbering_parameter of tag_t option * string * string
	| Invalid_extra_floatation_parameter of tag_t option * string * string
	| Invalid_extra_lang_parameter of tag_t option * string * string
	| Invalid_extra_classname_parameter of tag_t option * string * string
	| Invalid_extra_unknown_parameter of tag_t option * int * string
	| Invalid_extra_no_solutions of tag_t option * string
	| Invalid_extra_multiple_solutions of tag_t option * string

	| Invalid_name_entity of string
	| Invalid_deci_entity of string
	| Invalid_hexa_entity of string

	| Invalid_macro_argument_context
	| Invalid_macro_argument_number of string * int
	| Invalid_macro_call of ref_t * int * int
	| Invalid_macro_reference of ref_t
	| Invalid_macro_depth of ref_t * int

	| Invalid_language of tag_t option * string
	| Invalid_mathtex of tag_t option * string
	| Invalid_mathml of tag_t option * string
	| Invalid_column_number of tag_t option * int * int * int
	| Invalid_column_specifier of tag_t option * string
	| Invalid_cell_specifier of tag_t option * string
	| Invalid_feature of tag_t option * string

	| Duplicate_label of tag_t option * ref_t
	| Empty_target of tag_t option * ref_t
	| Wrong_target of tag_t option * target_t * target_t * ref_t
	| Absent_target of tag_t option * ref_t

	| Malformed_code_point
	| Reading_error of string

	with sexp


(**	An error is a pair consisting of the context where the error
	occurred and the error message itself.
*)
type t = error_context_t * error_msg_t with sexp

