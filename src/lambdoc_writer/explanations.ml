(********************************************************************************)
(*	Implementation file for Explain_error module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Explains errors.
*)

open Printf
open Lambdoc_core


(********************************************************************************)
(**	{2 Functions and values}						*)
(********************************************************************************)

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
	| Some tag	-> sprintf "command '%s'" tag
	| None		-> "anonymous command"


let explain_error = function

	| Error.Bad_label_parameter (tag, reason) ->
		let exp_reason = explain_reason "a" "label" reason
		in sprintf "Invalid labelling for %s: %s." (explain_tag tag) exp_reason

	| Error.Bad_order_parameter (tag, reason) ->
		let exp_reason = explain_reason "an" "order" reason
		in sprintf "Invalid ordering for %s: %s." (explain_tag tag) exp_reason

	| Error.Bad_extra_parameter (tag, reason) ->
		let exp_reason = explain_reason "an" "extra" reason
		in sprintf "Invalid extra parameter for %s: %s." (explain_tag tag) exp_reason

	| Error.Bad_secondary_parameter (tag, reason) ->
		let exp_reason = explain_reason "a" "secondary" reason
		in sprintf "Invalid secondary parameter for %s: %s." (explain_tag tag) exp_reason

	| Error.Unknown_env_command tag ->
		sprintf "Unknown environment command '%s'." tag

	| Error.Unknown_simple_command tag ->
		sprintf "Unknown simple '%s'." tag

	| Error.Invalid_extra_boolean_parameter (tag, key, value) ->
		sprintf "In %s, the key '%s' expects a boolean parameter, yet the assigned value '%s' cannot be interpreted as such." (explain_tag tag) key value

	| Error.Invalid_extra_numeric_parameter (tag, key, value) ->
		sprintf "In %s, the key '%s' expects a numeric parameter, yet the assigned value '%s' cannot be interpreted as such." (explain_tag tag) key value

	| Error.Invalid_extra_bullet_parameter (tag, key, value) ->
		sprintf "In %s, the key '%s' expects a bullet specifier, yet the assigned value '%s' cannot be interpreted as such." (explain_tag tag) key value

	| Error.Invalid_extra_numbering_parameter (tag, key, value) ->
		sprintf "In %s, the key '%s' expects a numbering specifier, yet the assigned value '%s' cannot be interpreted as such." (explain_tag tag) key value

	| Error.Invalid_extra_alignment_parameter (tag, key, value) ->
		sprintf "In %s, the key '%s' expects a alignment specifier, yet the assigned value '%s' cannot be interpreted as such." (explain_tag tag) key value

	| Error.Invalid_extra_lang_parameter (tag, key, value) ->
		sprintf "In %s, the key '%s' expects a language specifier, yet the assigned value '%s' cannot be interpreted as such." (explain_tag tag) key value

	| Error.Invalid_extra_unknown_parameter (tag, col, field) ->
		sprintf "In %s, the value '%s' assigned to field %d of the extra parameter cannot be interpreted." (explain_tag tag) field col

	| Error.Invalid_language (tag, lang) ->
		sprintf "Unknown language '%s' for %s." lang (explain_tag tag)

	| Error.Invalid_mathtex (tag, txt) ->
		sprintf "Invalid mathtex expression '%s' in %s." txt (explain_tag tag)

	| Error.Invalid_mathml (tag, txt) ->
		sprintf "Invalid mathml expression '%s' in %s." txt (explain_tag tag)

	| Error.Invalid_column_number (tag, linenum, found, expected) ->
		sprintf "Wrong number of columns for a row belonging to the %s started in line %d: found %d but expected %d columns." (explain_tag tag) linenum found expected

	| Error.Invalid_column_specifier (tag, spec) ->
		sprintf "Unknown column specifier '%c' in %s.  Valid column specifiers are c/C (for centred columns), l/L (for left aligned columns), r/R (for right aligned columns), and j/J (for justified columns)." spec (explain_tag tag)

	| Error.Invalid_feature (tag, description) ->
		sprintf "The feature '%s' requested by %s has been flagged as invalid for this document." description (explain_tag tag)

	| Error.Duplicate_label (tag, label) ->
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

	| Error.Absent_target (tag, label) ->
		sprintf "Reference to an undefined label '%s' in %s." label (explain_tag tag)

	| Error.Syntax_error ->
		"Syntax error."

