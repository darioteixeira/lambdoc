(********************************************************************************)
(*	Explanations.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Explains errors.
*)

open Printf
open Lambdoc_core


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

let explain_nesting blk =
	let expect which = "The document nesting rules expect a " ^ which ^ " block in this location"
	in match blk with
		| `Any_blk	 -> "The document nesting rules forbid this block in this location"
		| `Paragraph_blk -> expect "'paragraph'"
		| `Decor_blk	 -> expect "'image' or 'verbatim'"
		| `Equation_blk	 -> expect "'mathtext' or 'mathml'"
		| `Printout_blk	 -> expect "'source'"
		| `Table_blk	 -> expect "'tabular'"
		| `Figure_blk	 -> expect "'image', 'verbatim', or 'subpage'"


let explain_reason article what = function

	| Error.Reason_is_empty_when_non_empty_mandatory ->
		sprintf "you provided an empty %s parameter, but in this context it should contain something" what

	| Error.Reason_is_empty_when_forbidden ->
		sprintf "you provided an empty %s parameter, but in this context it is altogether forbidden for this command" what

	| Error.Reason_is_non_empty_when_forbidden str ->
		sprintf "you provided %s %s parameter '%s', but in this context it is forbidden for this command" article what str

	| Error.Reason_is_absent_when_mandatory ->
		sprintf "you have not provided %s %s parameter, but in this context it is mandatory for this command" article what


let explain_tag = function
	| Some tag -> sprintf "command '%s'" tag
	| None	   -> "anonymous command"


let explain_error = function

	| Error.Invalid_label_parameter (tag, reason) ->
		let exp_reason = explain_reason "a" "label" reason
		in sprintf "Invalid labelling for %s: %s." (explain_tag tag) exp_reason

	| Error.Invalid_order_parameter (tag, reason) ->
		let exp_reason = explain_reason "an" "order" reason
		in sprintf "Invalid ordering for %s: %s." (explain_tag tag) exp_reason

	| Error.Invalid_extra_parameter (tag, reason) ->
		let exp_reason = explain_reason "an" "extra" reason
		in sprintf "Invalid extra parameter for %s: %s." (explain_tag tag) exp_reason

	| Error.Nested_link tag ->
		sprintf "Nested link in %s." (explain_tag tag)

	| Error.Empty_listing tag ->
		sprintf "Empty listing in %s." (explain_tag tag)

	| Error.Unexpected_block (tag, blk) ->
		sprintf "Unexpected block %s. %s." (explain_tag tag) (explain_nesting blk)

	| Error.Invalid_extra_boolean_parameter (tag, key, value) ->
		sprintf "In %s, the key '%s' expects a boolean parameter, yet the assigned value '%s' cannot be interpreted as such." (explain_tag tag) key value

	| Error.Invalid_extra_numeric_parameter (tag, key, value) ->
		sprintf "In %s, the key '%s' expects a numeric parameter, yet the assigned value '%s' cannot be interpreted as such." (explain_tag tag) key value

	| Error.Invalid_extra_bullet_parameter (tag, key, value) ->
		sprintf "In %s, the key '%s' expects a bullet specifier, yet the assigned value '%s' cannot be interpreted as such." (explain_tag tag) key value

	| Error.Invalid_extra_numbering_parameter (tag, key, value) ->
		sprintf "In %s, the key '%s' expects a numbering specifier, yet the assigned value '%s' cannot be interpreted as such." (explain_tag tag) key value

	| Error.Invalid_extra_floatation_parameter (tag, key, value) ->
		sprintf "In %s, the key '%s' expects a floatation specifier, yet the assigned value '%s' cannot be interpreted as such." (explain_tag tag) key value

	| Error.Invalid_extra_lang_parameter (tag, key, value) ->
		sprintf "In %s, the key '%s' expects a language specifier, yet the assigned value '%s' cannot be interpreted as such." (explain_tag tag) key value

	| Error.Invalid_extra_unknown_parameter (tag, col, field) ->
		sprintf "In %s, the value '%s' assigned to field %d of the extra parameter cannot be interpreted." (explain_tag tag) field col

	| Error.Invalid_extra_no_solutions (tag, extra) ->
		sprintf "In %s, no conclusive assignment can be made for the extra parameter '%s'." (explain_tag tag) extra

	| Error.Invalid_extra_multiple_solutions (tag, extra) ->
		sprintf "In %s, the extra parameters '%s' cannot be interpreted unambiguously." (explain_tag tag) extra

	| Error.Invalid_name_entity ent ->
		sprintf "Unknown entity '%s'." ent

	| Error.Invalid_deci_entity ent ->
		sprintf "Invalid Unicode decimal code point '%s'." ent

	| Error.Invalid_hexa_entity ent ->
		sprintf "Invalid Unicode hexadecimal code point '%s'." ent

	| Error.Invalid_macro_argument_context ->
		"Invalid context for reference to a macro argument.  It may only be used inside a macro definition."

	| Error.Invalid_macro_argument_number (found, expected) ->
		let correct = match expected with
			| 0 -> "This macro takes no arguments"
			| 1 -> "This macro takes one argument referenced by the integer 1"
			| x -> "This macro's arguments must be referenced by an integer ranging from 1 to " ^ (string_of_int x)
		in sprintf "Invalid macro argument '%s'.  %s." found correct

	| Error.Invalid_macro_call (name, found, expected) ->
		sprintf "Invalid macro invocation.  Macro '%s' expects %d argument(s) but found %d instead." name expected found

	| Error.Duplicate_macro (tag, name) ->
		sprintf "The definition of macro '%s' in '%s' duplicates a previously defined macro." name (explain_tag tag)

	| Error.Undefined_macro (tag, name) ->
		sprintf "Reference to undefined macro '%s'.  Remember that macros must be defined before they are referenced and a macro may not invoke itself." name

	| Error.Duplicate_custom (tag, env) ->
		sprintf "The definition of custom environment '%s' in '%s' duplicates a previously defined environment." env (explain_tag tag)

	| Error.Undefined_custom (tag, env) ->
		sprintf "The environment '%s' used in '%s' has not been defined yet." env (explain_tag tag)

	| Error.Invalid_counter (tag, counter) ->
		sprintf "The counter '%s' requested in '%s' has been already assigned to a different class of custom environment." counter (explain_tag tag)

	| Error.Invalid_mathtex (tag, txt) ->
		sprintf "Invalid mathtex expression '%s' in %s." txt (explain_tag tag)

	| Error.Invalid_mathml (tag, txt) ->
		sprintf "Invalid mathml expression '%s' in %s." txt (explain_tag tag)

	| Error.Invalid_column_number (tag, linenum, found, expected) ->
		sprintf "Wrong number of columns for a row belonging to the %s started in line %d: found %d but expected %d columns." (explain_tag tag) linenum found expected

	| Error.Invalid_column_specifier (tag, spec) ->
		sprintf "Unknown column specifier '%s' in %s.  Valid column specifiers are c/C (for centred columns), l/L (for left aligned columns), r/R (for right aligned columns), and j/J (for justified columns)." spec (explain_tag tag)

	| Error.Invalid_cell_specifier (tag, spec) ->
		sprintf "Invalid cell specifier '%s' in %s.  Cell specifiers should consist of an integer indicating the span followed by a single character (either c/C, l/L, r/R, j/J) indicating the alignment." spec (explain_tag tag)

	| Error.Invalid_feature (tag, description) ->
		sprintf "The feature '%s' requested by %s has been flagged as invalid for this document." description (explain_tag tag)

	| Error.Duplicate_target (tag, label) ->
		sprintf "Attempt to redefine label '%s' in %s." label (explain_tag tag)

	| Error.Empty_target (tag, label) ->
		sprintf "Empty target for %s and label '%s'." (explain_tag tag) label

	| Error.Wrong_target (tag, expected, suggested, label) ->
		let str_expected = match expected with
			| Error.Target_bib    -> "bibliography notes"
			| Error.Target_note   -> "note definitions"
			| Error.Target_label  -> "document labels"
		and str_suggested = match suggested with
			| Error.Target_bib    -> "'\\cite'"
			| Error.Target_note   -> "'\\see'"
			| Error.Target_label  -> "'\\ref', '\\sref', or '\\mref'"
		in sprintf ("Wrong target '%s' for %s: this command should only be used to reference %s.  Considering your target, perhaps you mean to use command %s instead?") label (explain_tag tag) str_expected str_suggested

	| Error.Undefined_target (tag, label) ->
		sprintf "Reference to an undefined label '%s' in %s." label (explain_tag tag)

	| Error.Malformed_code_point ->
		sprintf "This line contains a malformed UTF-8 code point (represented by the character '\xef\xbf\xbd')."

	| Error.Reading_error msg ->
		sprintf "%s." msg

