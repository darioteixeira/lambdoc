(********************************************************************************)
(*  Lambdoc_writer_explanations.ml
    Copyright (c) 2009-2015 Dario Teixeira (dario.teixeira@nleyten.com)
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Explains errors.
*)

module String = BatString
module Emblang = Lambdoc_writer_emblang

open Printf
open Lambdoc_core
open Basic


(********************************************************************************)
(** {1 Private functions and values}                                            *)
(********************************************************************************)

let escape str =
    let replacer = function
        | '\\' -> "\\\\"
        | '#'  -> "\\#"
        | x    -> String.of_char x
    in String.replace_chars replacer str


let explain_nesting blk =
    let which = match blk with
        | `Super_blk      -> "a super block (or sub-types)"
        | `Listable_blk   -> "a listable block (or sub-types)"
        | `Quotable_blk   -> "a quotable block (or sub-types)"
        | `Embeddable_blk -> "an embeddable block"
        | `Paragraph_blk  -> "a 'paragraph' block"
        | `Equation_blk   -> "a 'mathtex' or 'mathml' block"
        | `Printout_blk   -> "a 'source' block"
        | `Table_blk      -> "a 'tabular' block"
        | `Figure_blk     -> "a 'picture', 'verbatim', or 'subpage' block"
    in "The document nesting rules expect " ^ which ^ " in this location"


let explain_reason article what = function

    | Error.Reason_is_empty_when_non_empty_mandatory ->
        sprintf "you provided an empty %s parameter, but in this context it should contain something" what

    | Error.Reason_is_empty_when_forbidden ->
        sprintf "you provided an empty %s parameter, but in this context it is altogether forbidden for this command" what

    | Error.Reason_is_non_empty_when_forbidden str ->
        sprintf "you provided %s %s parameter '#%s#', but in this context it is forbidden for this command" article what (escape str)

    | Error.Reason_is_absent_when_mandatory ->
        sprintf "you have not provided %s %s parameter, but in this context it is mandatory for this command" article what


let explain_tag = function
    | Some tag -> sprintf "command '#%s#'" (escape tag)
    | None     -> "anonymous command"


let explain_with_colon what =
    sprintf "%s must begin with a lowercase Roman letter, and is optionally followed by lowercase Roman letters, digits, or the characters '#:#' (colon), '#-#' (dash), and '#_#' (underscore)" what


let explain_without_colon what =
    sprintf "%s must begin with a lowercase Roman letter, and is optionally followed by lowercase Roman letters, digits, or the characters '#-#' (dash), and '#_#' (underscore)" what


let explain_ident what =
    sprintf "%s must begin with a lowercase Roman letter, and is optionally followed by lowercase Roman letters, digits, or the character '#_#' (underscore)" what


let explain_level level = (level : Level.section :> int)


let explain_wrapper = function
    | Wrapper.Equation -> "an equation"
    | Wrapper.Printout -> "a printout"
    | Wrapper.Table    -> "a table"
    | Wrapper.Figure   -> "a figure"


let explain_message = function

    | Error.Misplaced_label_parameter reason ->
        let exp_reason = explain_reason "a" "label" reason in
        sprintf "Misplaced label parameter: %s." exp_reason

    | Error.Misplaced_order_parameter reason ->
        let exp_reason = explain_reason "an" "order" reason in
        sprintf "Misplaced order parameter: %s." exp_reason

    | Error.Invalid_label label ->
        sprintf "Invalid label '#%s#'. %s." (escape label) (explain_with_colon "A label")

    | Error.Invalid_order_format order ->
        sprintf "Unable to interpret the string '#%s#' as an ordering." (escape order)

    | Error.Invalid_order_levels (order, expected, found) ->
        sprintf "Expected %d hierarchical levels in the ordering for this command, but the string '#%s#' contains %d instead." (explain_level expected) (escape order) found

    | Error.Invalid_style_bad_boolean (key, value) ->
        sprintf "In the style parameters for this command, the key '#%s#' expects a boolean parameter, yet the assigned value '#%s#' cannot be interpreted as such. Valid boolean values are 'true'/'on'/'yes' or 'false'/'off'/'no'." (escape key) (escape value)

    | Error.Invalid_style_bad_lang (key, value) ->
        sprintf "In the style parameters for this command, the key '#%s#' expects a language specifier, yet the assigned value '#%s#' cannot be interpreted as such. Valid languages are all those accepted by the Camlhighlight library." (escape key) (escape value)

    | Error.Invalid_style_bad_numeric (key, value, low, high) ->
        sprintf "In the style parameters for this command, the key '#%s#' expects an integer x such that %d <= x <= %d, yet the assigned value '#%s#' cannot be interpreted as such." (escape key) low high (escape value)

    | Error.Invalid_style_bad_colsfmt (key, spec) ->
        sprintf "In the style parameters for this command, the key '#%s#' expects a columns specification, yet the assigned value '#%s#' cannot be interpreted as such. Valid column specifiers are c/C (for centred columns), l/L (for left aligned columns), r/R (for right aligned columns), and j/J (for justified columns)." (escape key) spec

    | Error.Invalid_style_bad_cellfmt (key, spec) ->
        sprintf "In the style parameters for this command, the key '#%s#' expects a cell format specification, yet the assigned value '#%s#' cannot be interpreted as such. A cell format should consist of an integer indicating the span, followed by a single character (either c/C, l/L, r/R, j/J) indicating the alignment." (escape key) spec

    | Error.Invalid_style_bad_classname str ->
        sprintf "In the style parameters for this command, the assigned value '#%s#' cannot be interpreted as a classname. %s." (escape str) (explain_without_colon "A classname")

    | Error.Invalid_style_bad_keyvalue str ->
        sprintf "In the style parameters for this command, the assigned value '#%s#' cannot be interpreted as a key/value pair." (escape str)

    | Error.Invalid_style_misplaced_keyvalue (key, value) ->
        sprintf "In the style parameters for this command, the key/value pair '#%s#=#%s#' is not accepted for this particular command." (escape key) (escape value)

    | Error.Invalid_style_misplaced_classname str ->
        sprintf "In the style parameters for this command, the assigned classname '#%s#' is not part of the whitelist for this particular command." (escape str)

    | Error.Invalid_style_unknown_keyvalue (key, value) ->
        sprintf "In the style parameters for this command, the key/value pair '#%s#=#%s#' cannot be interpreted." (escape key) (escape value)

    | Error.Invalid_entity_name ent ->
        sprintf "Unknown entity '#%s#'." (escape ent)

    | Error.Invalid_entity_deci ent ->
        sprintf "Invalid Unicode decimal code point '#%s#'." (escape ent)

    | Error.Invalid_entity_hexa ent ->
        sprintf "Invalid Unicode hexadecimal code point '#%s#'." (escape ent)

    | Error.Invalid_macro_nargs (name, nargs) ->
        sprintf "Invalid number of parameters '#%s#' for macro '#%s#'. Please provide an integer." (escape nargs) name

    | Error.Invalid_macro_argument_context ->
        "Invalid context for reference to a macro argument. It may only be used inside a macro definition."

    | Error.Invalid_macro_argument_number (found, expected) ->
        let correct = match expected with
            | 0 -> "This macro takes no arguments"
            | 1 -> "This macro takes one argument referenced by the integer 1"
            | x -> "This macro's arguments must be referenced by an integer ranging from 1 to " ^ (string_of_int x)
        in sprintf "Invalid macro argument '%s' for macro. %s." (escape found) correct

    | Error.Invalid_macro_call (name, found, expected) ->
        sprintf "Invalid macro invocation. Macro '#%s#' expects %d argument(s) but found %d instead." (escape name) expected found

    | Error.Invalid_macro name ->
        sprintf "Invalid macro name '#%s#'. %s." (escape name) (explain_ident "A macro")

    | Error.Duplicate_macro name ->
        sprintf "The definition of macro '#%s#' duplicates a previously defined macro." (escape name)

    | Error.Undefined_macro name ->
        sprintf "Reference to undefined macro '#%s#'. Remember that macros must be defined before they are referenced and a macro may not invoke itself." (escape name)

    | Error.Excessive_macro_depth max ->
        sprintf "Invocation of this command would cause depth of macro calls to go above maximum of %d." max

    | Error.Excessive_inline_depth max ->
        sprintf "Invocation of this command would cause depth of inline elements to go above maximum of %d." max

    | Error.Excessive_block_depth max ->
        sprintf "Invocation of this command would cause depth of block elements to go above maximum of %d." max

    | Error.Invalid_custom env ->
        sprintf "Invalid name '#%s#' for custom environment. %s." (escape env) (explain_ident "A custom environment")

    | Error.Mismatched_custom (env, found, expected) ->
        let explain_custom = function
            | Custom.Boxout  -> "boxout"
            | Custom.Theorem -> "theorem"
        in sprintf "The custom environment '#%s#' is used as a %s, but it was previously defined as a %s." (escape env) (explain_custom found) (explain_custom expected)

    | Error.Duplicate_custom env ->
        sprintf "The definition of custom environment '#%s#' duplicates a previously defined environment." (escape env)

    | Error.Undefined_custom env ->
        sprintf "The environment '#%s#' used in this command has not been defined yet." (escape env)

    | Error. Invalid_wrapper kind ->
        sprintf "You provided an empty ordering for %s without a caption. You must either discard the empty ordering specification or provide a caption." (explain_wrapper kind)

    | Error. Invalid_section_level level ->
        sprintf "You requested a section level of %d, but the allowed maximum is %d." level Level.max_section

    | Error. Invalid_title_level level ->
        sprintf "You requested a title level of %d, but the allowed maximum is %d." level Level.max_title

    | Error.Invalid_counter counter ->
        sprintf "Invalid name '#%s#' for counter. %s." (escape counter) (explain_with_colon "A counter")

    | Error.Mismatched_counter counter ->
        sprintf "The counter '#%s#' has been already assigned to a different class of custom environment." (escape counter)

    | Error.Unexpected_counter counter ->
        sprintf "You have requested counter '#%s#', but custom environments without a title may not have an associated counter." (escape counter)

    | Error.Invalid_mathtex txt ->
        sprintf "Invalid MathTeX expression '#%s#'." (escape txt)

    | Error.Invalid_mathml txt ->
        sprintf "Invalid MathML expression '#%s#'." (escape txt)

    | Error.Invalid_column_number (orig_tag, orig_linenum, found, expected) ->
        sprintf "Wrong number of columns for row: found %d but expected %d columns. This row belongs to the tabular environment declared in line %d by %s" found expected orig_linenum (explain_tag orig_tag)

    | Error.Mismatched_column_numbers cols_per_row ->
            sprintf "In this tabular environment not all rows have the same number of columns. The number of columns per row is [%s]" (List.map string_of_int cols_per_row |> String.join "; ")

    | Error.Duplicate_target label ->
        sprintf "Attempt to redefine label '#%s#'." (escape label)

    | Error.Empty_target label ->
        sprintf "Empty target for label '#%s#'." (escape label)

    | Error.Wrong_target (label, expected, suggested) ->
        let str_expected = match expected with
            | Error.Target_bib   -> "bibliography notes"
            | Error.Target_note  -> "note definitions"
            | Error.Target_label -> "document labels"
        and str_suggested = match suggested with
            | Error.Target_bib   -> "'#\\cite'#"
            | Error.Target_note  -> "'#\\see'#"
            | Error.Target_label -> "'#\\ref#', '#\\sref#', or '#\\mref#'"
        in sprintf ("Wrong target '#%s#'. This command should only be used to reference %s. Considering your target, perhaps you mean to use command %s instead?") (escape label) str_expected str_suggested

    | Error.Undefined_target label ->
        sprintf "Reference to an undefined label '#%s#'." (escape label)

    | Error.Empty_source ->
        sprintf "Empty source."

    | Error.Empty_verbatim ->
        sprintf "Empty verbatim."

    | Error.Empty_list ->
        sprintf "Empty list."

    | Error.Empty_sequence ->
        sprintf "Empty inline sequence."

    | Error.Empty_fragment ->
        sprintf "Empty fragment."

    | Error.Unexpected_inline ->
        sprintf "Unexpected command for inline sequence. Remember that nested links are not allowed!"

    | Error.Unexpected_block blk ->
        sprintf "Unexpected block command. %s." (explain_nesting blk)

    | Error.Missing_bibliography ->
        sprintf "This document cites bibliography entries but does not declare a bibliography section."

    | Error.Missing_notes ->
        sprintf "This document references end notes but does not declare a notes section."

    | Error.Malformed_code_point ->
        sprintf "This line contains at least one malformed UTF-8 code point (represented by the character '#\xef\xbf\xbd#')."

    | Error.Reading_error msg ->
        sprintf "%s." (escape msg)

    | Error.Unavailable_feature description ->
        sprintf "The feature '%s' requested by this command is unavailable for this document." description

    | Error.Extension_error msg ->
        sprintf "%s." (escape msg)


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

let explain (_, error_tag, error_msg) =
    Printf.sprintf "Error in %s. %s" (explain_tag error_tag) (explain_message error_msg) |> Emblang.convert

