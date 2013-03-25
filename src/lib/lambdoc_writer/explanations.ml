(********************************************************************************)
(*	Explanations.ml
	Copyright (c) 2009-2013 Dario Teixeira (dario.teixeira#yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Explains errors.
*)

open Printf
open Lambdoc_core

module String = BatString


(********************************************************************************)
(**	{1 Private functions and values}					*)
(********************************************************************************)

let escape str =
	let replacer = function
		| '\\' -> "\\\\"
		| '#'  -> "\\#"
		| x    -> String.of_char x
	in String.replace_chars replacer str


let explain_nesting blk =
	let which = match blk with
		| `Super_blk	  -> "a super block (or sub-types)"
		| `Listable_blk	  -> "a listable block (or sub-types)"
		| `Quotable_blk	  -> "a quotable block (or sub-types)"
		| `Embeddable_blk -> "an embeddable block"
		| `Paragraph_blk  -> "a 'paragraph' block"
		| `Equation_blk	  -> "a 'mathtex' or 'mathml' block"
		| `Printout_blk	  -> "a 'source' block"
		| `Table_blk	  -> "a 'tabular' block"
		| `Figure_blk	  -> "a 'picture', 'verbatim', or 'subpage' block"
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
	| None	   -> "anonymous command"


let explain_with_colon what =
	sprintf "%s must begin with a lowercase Roman letter, and is optionally followed by lowercase Roman letters, digits, or the characters '#:#' (colon), '#-#' (dash), and '#_#' (underscore)" what


let explain_without_colon what =
	sprintf "%s must begin with a lowercase Roman letter, and is optionally followed by lowercase Roman letters, digits, or the characters '#-#' (dash), and '#_#' (underscore)" what


let explain_ident what =
	sprintf "%s must begin with a lowercase Roman letter, and is optionally followed by lowercase Roman letters, digits, or the character '#_#' (underscore)" what


let explain_level = function
	| `Level1 -> 1
	| `Level2 -> 2
	| `Level3 -> 3


let explain_wrapper = function
	| Wrapper.Equation -> "an equation"
	| Wrapper.Printout -> "a printout"
	| Wrapper.Table    -> "a table"
	| Wrapper.Figure   -> "a figure"


let explain_error = function

	| Error.Misplaced_label_parameter (tag, reason) ->
		let exp_reason = explain_reason "a" "label" reason in
		sprintf "Misplaced label parameter for %s: %s." (explain_tag tag) exp_reason

	| Error.Misplaced_order_parameter (tag, reason) ->
		let exp_reason = explain_reason "an" "order" reason in
		sprintf "Misplaced order parameter for %s: %s." (explain_tag tag) exp_reason

	| Error.Invalid_label (tag, label) ->
		sprintf "Invalid label '#%s#' in %s. %s." (escape label) (explain_tag tag) (explain_with_colon "A label")

	| Error.Invalid_order_format (tag, order) ->
		sprintf "Unable to interpret the string '#%s#' in %s as an ordering." (escape order) (explain_tag tag)

	| Error.Invalid_order_levels (tag, order, expected, found) ->
		sprintf "Expected %d hierarchical levels in the ordering for %s, but the string '#%s#' contains %d instead." (explain_level expected) (explain_tag tag) (escape order) found

	| Error.Invalid_style_bad_boolean (tag, key, value) ->
		sprintf "In the style parameters of %s, the key '#%s#' expects a boolean parameter, yet the assigned value '#%s#' cannot be interpreted as such.  Valid boolean values are 'true'/'on'/'yes' or 'false'/'off'/'no'." (explain_tag tag) (escape key) (escape value)

	| Error.Invalid_style_bad_coversize (tag, key, value) ->
		sprintf "In the style parameters of %s, the key '#%s#' expects a cover size specifier, yet the assigned value '#%s#' cannot be interpreted as such. Valid cover size values are 'small', 'medium', and 'large'." (explain_tag tag) (escape key) (escape value)

	| Error.Invalid_style_bad_lang (tag, key, value) ->
		sprintf "In the style parameters of %s, the key '#%s#' expects a language specifier, yet the assigned value '#%s#' cannot be interpreted as such.  Valid languages are all those accepted by the Camlhighlight library." (explain_tag tag) (escape key) (escape value)

	| Error.Invalid_style_bad_numeric (tag, key, value, low, high) ->
		sprintf "In the style parameters of %s, the key '#%s#' expects an integer x such that %d <= x <= %d, yet the assigned value '#%s#' cannot be interpreted as such." (explain_tag tag) (escape key) low high (escape value)

	| Error.Invalid_style_bad_classname (tag, str) ->
		sprintf "In the style parameters of %s, the assigned value '#%s#' cannot be interpreted as a classname. %s." (explain_tag tag) (escape str) (explain_without_colon "A classname")

	| Error.Invalid_style_bad_keyvalue (tag, str) ->
		sprintf "In the style parameters of %s, the assigned value '#%s#' cannot be interpreted as a key/value pair." (explain_tag tag) (escape str)

	| Error.Invalid_style_misplaced_keyvalue (tag, key, value) ->
		sprintf "In the style parameters of %s, the key/value pair '#%s#=#%s#' is not accepted for this particular command." (explain_tag tag) (escape key) (escape value)

	| Error.Invalid_style_misplaced_classname (tag, str) ->
		sprintf "In the style parameters of %s, the assigned classname '#%s#' is not part of the whitelist for this particular command." (explain_tag tag) (escape str)

	| Error.Invalid_style_unknown_keyvalue (tag, key, value) ->
		sprintf "In the style parameters of %s, the key/value pair '#%s#=#%s#' cannot be interpreted." (explain_tag tag) (escape key) (escape value)

	| Error.Invalid_entity_name ent ->
		sprintf "Unknown entity '#%s#'." (escape ent)

	| Error.Invalid_entity_deci ent ->
		sprintf "Invalid Unicode decimal code point '#%s#'." (escape ent)

	| Error.Invalid_entity_hexa ent ->
		sprintf "Invalid Unicode hexadecimal code point '#%s#'." (escape ent)

	| Error.Invalid_macro_nargs (name, nargs) ->
		sprintf "Invalid number of parameters '#%s#' for macro '#%s#'.  Please provide an integer." (escape nargs) name

	| Error.Invalid_macro_argument_context ->
		"Invalid context for reference to a macro argument.  It may only be used inside a macro definition."

	| Error.Invalid_macro_argument_number (found, expected) ->
		let correct = match expected with
			| 0 -> "This macro takes no arguments"
			| 1 -> "This macro takes one argument referenced by the integer 1"
			| x -> "This macro's arguments must be referenced by an integer ranging from 1 to " ^ (string_of_int x)
		in sprintf "Invalid macro argument '%s'.  %s." (escape found) correct

	| Error.Invalid_macro_call (name, found, expected) ->
		sprintf "Invalid macro invocation.  Macro '#%s#' expects %d argument(s) but found %d instead." (escape name) expected found

	| Error.Invalid_macro (tag, name) ->
		sprintf "Invalid macro name '#%s#' in %s. %s." (escape name) (explain_tag tag) (explain_ident "A macro")

	| Error.Duplicate_macro (tag, name) ->
		sprintf "The definition of macro '#%s#' in %s duplicates a previously defined macro." (escape name) (explain_tag tag)

	| Error.Undefined_macro (tag, name) ->
		sprintf "Reference to undefined macro '#%s#'.  Remember that macros must be defined before they are referenced and a macro may not invoke itself." (escape name)

	| Error.Excessive_macro_depth (tag, max) ->
		sprintf "Invocation of %s would cause depth of macro calls to go above maximum of %d." (explain_tag tag) max

	| Error.Excessive_inline_depth (tag, max) ->
		sprintf "Invocation of %s would cause depth of inline elements to go above maximum of %d." (explain_tag tag) max

	| Error.Excessive_block_depth (tag, max) ->
		sprintf "Invocation of %s would cause depth of block elements to go above maximum of %d." (explain_tag tag) max

	| Error.Invalid_custom (tag, env) ->
		sprintf "Invalid name '#%s#' for custom environment in %s. %s." (escape env) (explain_tag tag) (explain_ident "A custom environment")

	| Error.Mismatched_custom (tag, env, found, expected) ->
		let explain_custom = function
			| Custom.Boxout  -> "boxout"
			| Custom.Theorem -> "theorem"
		in sprintf "In %s, the custom environment '#%s#' is used as a %s, but it was previously defined as a %s." (explain_tag tag) (escape env) (explain_custom found) (explain_custom expected)

	| Error.Duplicate_custom (tag, env) ->
		sprintf "The definition of custom environment '#%s#' in %s duplicates a previously defined environment." (escape env) (explain_tag tag)

	| Error.Undefined_custom (tag, env) ->
		sprintf "The environment '#%s#' used in %s has not been defined yet." (escape env) (explain_tag tag)

	| Error. Invalid_wrapper (tag, kind) ->
		sprintf "In %s, you provided an empty ordering for %s without a caption.  You must either discard the empty ordering specification or provide a caption." (explain_tag tag) (explain_wrapper kind)

	| Error.Invalid_counter (tag, counter) ->
		sprintf "Invalid name '#%s#' for counter in %s. %s." (escape counter) (explain_tag tag) (explain_with_colon "A counter")

	| Error.Mismatched_counter (tag, counter) ->
		sprintf "The counter '#%s#' requested in %s has been already assigned to a different class of custom environment." (escape counter) (explain_tag tag)

	| Error.Unexpected_counter (tag, counter) ->
		sprintf "You have requested counter '#%s#' for %s, but custom environments without a title may not have an associated counter." (escape counter) (explain_tag tag)

	| Error.Invalid_mathtex (tag, txt) ->
		sprintf "Invalid MathTeX expression '#%s#' in %s." (escape txt) (explain_tag tag)

	| Error.Invalid_mathml (tag, txt) ->
		sprintf "Invalid MathML expression '#%s#' in %s." (escape txt) (explain_tag tag)

	| Error.Invalid_column_number (tag, orig_tag, orig_linenum, found, expected) ->
		sprintf "Wrong number of columns for a row declared by %s: found %d but expected %d columns.  This row belongs to the tabular environment declared in line %d by %s" (explain_tag tag) found expected orig_linenum (explain_tag orig_tag)

	| Error.Invalid_column_specifier (tag, spec) ->
		sprintf "Unknown column specifier '#%s#' in %s.  Valid column specifiers are c/C (for centred columns), l/L (for left aligned columns), r/R (for right aligned columns), and j/J (for justified columns)." (escape spec) (explain_tag tag)

	| Error.Invalid_cell_specifier (tag, spec) ->
		sprintf "Invalid cell specifier '#%s#' in %s.  Cell specifiers should consist of an integer indicating the span, followed by a single character (either c/C, l/L, r/R, j/J) indicating the alignment." (escape spec) (explain_tag tag)

	| Error.Duplicate_target (tag, label) ->
		sprintf "Attempt to redefine label '#%s#' in %s." (escape label) (explain_tag tag)

	| Error.Empty_target (tag, label) ->
		sprintf "Empty target for %s and label '#%s#'." (explain_tag tag) (escape label)

	| Error.Wrong_target (tag, label, expected, suggested) ->
		let str_expected = match expected with
			| Error.Target_bib    -> "bibliography notes"
			| Error.Target_note   -> "note definitions"
			| Error.Target_label  -> "document labels"
		and str_suggested = match suggested with
			| Error.Target_bib    -> "'#\\cite'#"
			| Error.Target_note   -> "'#\\see'#"
			| Error.Target_label  -> "'#\\ref#', '#\\sref#', or '#\\mref#'"
		in sprintf ("Wrong target '#%s#' for %s: this command should only be used to reference %s.  Considering your target, perhaps you mean to use command %s instead?") (escape label) (explain_tag tag) str_expected str_suggested

	| Error.Undefined_target (tag, label) ->
		sprintf "Reference to an undefined label '#%s#' in %s." (escape label) (explain_tag tag)

	| Error.Unavailable_bookmaker (tag, description) ->
		sprintf "No book information fetching is available for feature '%s' requested by %s." description (explain_tag tag)

	| Error.Uncapable_bookmaker (tag, description, error) ->
		sprintf "Failure while fetching book information for feature '%s' requested by %s: %s." description (explain_tag tag) error

	| Error.Malformed_ISBN (tag, isbn) ->
		sprintf "The string '#%s#' in %s is not a valid ISBN." (escape isbn) (explain_tag tag)

	| Error.Unknown_ISBN (tag, isbn) ->
		sprintf "Cannot find book with ISBN '#%s#' in %s." (escape isbn) (explain_tag tag)

	| Error.Empty_source tag ->
		sprintf "Empty source environment in %s." (explain_tag tag)

	| Error.Empty_verbatim tag ->
		sprintf "Empty verbatim environment in %s." (explain_tag tag)

	| Error.Empty_list tag ->
		sprintf "Empty list in %s." (explain_tag tag)

	| Error.Empty_sequence tag ->
		sprintf "Empty inline sequence in %s." (explain_tag tag)

	| Error.Empty_fragment tag ->
		sprintf "Empty fragment in %s." (explain_tag tag)

	| Error.Unexpected_inline tag ->
		sprintf "Unexpected node %s in inline sequence.  Remember that nested links are not allowed!." (explain_tag tag)

	| Error.Unexpected_block (tag, blk) ->
		sprintf "Unexpected %s. %s." (explain_tag tag) (explain_nesting blk)

	| Error.Malformed_code_point ->
		sprintf "This line contains a malformed UTF-8 code point (represented by the character '#\xef\xbf\xbd#')."

	| Error.Reading_error msg ->
		sprintf "%s." (escape msg)

	| Error.Unavailable_feature (tag, description) ->
		sprintf "The feature '%s' requested by %s is unavailable for this document." description (explain_tag tag)


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

let explain error =
	Emblang.convert (explain_error error)

