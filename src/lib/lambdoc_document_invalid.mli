(** Definition of an invalid Lambdoc document.
*)

module Blkcat = Lambdoc_document_blkcat
module Valid = Lambdoc_document_valid

open Valid


(********************************************************************************)
(** {1 Document errors}                                                         *)
(********************************************************************************)

module Error:
sig
    (** Definition of the error context.  The context includes any number
        of lines preceding the error, the error line proper, and a number
        of lines that follow.
    *)
    type context =
        {
        error_line_number: int;         (** Number of the line where the error occurred. *)
        error_line_before: string list; (** Lines immediately before the error line. *)
        error_line_actual: string;      (** Contents of the line where the error is found. *)
        error_line_after: string list;  (** Lines immediately after the error line. *)
        } [@@deriving sexp]

    (** Reasons why a parameter can be invalid.
    *)
    type invalid_parameter_reason =
        | Reason_is_empty_when_non_empty_mandatory
        | Reason_is_empty_when_forbidden
        | Reason_is_non_empty_when_forbidden of string
        | Reason_is_absent_when_mandatory
        [@@deriving sexp]

    (** Expected targets.
    *)
    type target =
        | Target_bib
        | Target_note
        | Target_label
        [@@deriving sexp]

    (** The various types of error messages.
    *)
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

        | Invalid_column_number of ident option * int * int * int       (* (tabular_ident, tabular_linenum, found, expected) *)
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

    (** A contextualised error is a triple consisting of the context where
        the error occurred (where applicable), the offending command (also
        where applicable) and the error message itself.
    *)
    type contextualized = context option * ident option * msg [@@deriving sexp]

    (** A localized error is a triple consisting of the line number where
        the error occurred (where applicable), the offending command (also
        where applicable) and the error message itself.
    *)
    type localized = int option * ident option * msg

    (** A reading error is a triple consisting of the line number where
        the error occurred (where applicable), the offending command (also
        where applicable) and a string message.
    *)
    type reading = int option * ident option * string
end


(********************************************************************************)
(** {1 Invalid documents}                                                       *)
(********************************************************************************)

type t = Error.contextualized list [@@deriving sexp]

val make: Error.contextualized list -> t

val serialize: t -> string
val deserialize: string -> t

