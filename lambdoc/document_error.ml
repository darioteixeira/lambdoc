(********************************************************************************)
(**	Definition of document errors.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

TYPE_CONV_PATH "Document"

open Document_basic


(********************************************************************************)
(**	{2 Error module}							*)
(********************************************************************************)

module Error =
struct
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

	(**	The various types of error messages.
	*)
	type error_msg_t =
		| Bad_label_parameter of tag_t * invalid_parameter_reason_t
		| Bad_order_parameter of tag_t * invalid_parameter_reason_t
		| Bad_extra_parameter of tag_t * invalid_parameter_reason_t
		| Bad_secondary_parameter of tag_t * invalid_parameter_reason_t
		| Unknown_bullet_type of tag_t * string
		| Unknown_numbering_type of tag_t * string
		| Unknown_alignment_type of tag_t * string
		| Unknown_figure_type of tag_t * string
		| Unknown_math_type of tag_t * string
		| Unknown_setting of plain_t
		| Unknown_env_command of tag_t
		| Unknown_simple_command of tag_t
		| Duplicate_label of tag_t * ref_t
		| Duplicate_block of tag_t
		| Missing_block of tag_t * string
		| Invalid_block of tag_t * string
		| Invalid_column_specifier of tag_t * char
		| Invalid_mathtex of mathtex_t
		| Invalid_mathml of mathml_t
		| Wrong_column_number of int * int * int
		| Empty_target of tag_t * ref_t
		| Wrong_target of tag_t * target_t * target_t * ref_t
		| Absent_target of tag_t * ref_t
		| Invalid_feature of string * string
		| Syntax_error
		with sexp

	(**	An error is a pair consisting of the context where the error
		occurred and the error message itself.
	*)
	type t = error_context_t * error_msg_t with sexp
end

