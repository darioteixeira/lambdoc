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
	let which = match blk with
		| `Super_blk	  -> "a super block (or sub-types)"
		| `Listable_blk	  -> "a listable block (or sub-types)"
		| `Embeddable_blk -> "an embeddable block (or sub-types)"
		| `Textual_blk	  -> "a textual block"
		| `Paragraph_blk  -> "a 'paragraph' block"
		| `Decor_blk	  -> "an 'image' or 'verbatim' block"
		| `Equation_blk	  -> "a 'mathtex' or 'mathml' block"
		| `Printout_blk	  -> "a 'source' block"
		| `Table_blk	  -> "a 'tabular' block"
		| `Figure_blk	  -> "an 'image', 'verbatim', or 'subpage' block"
	in "The document nesting rules expect " ^ which ^ " in this location"


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


let explain_ident what =
	sprintf "%s must begin with a alphabetic letter, and is optionally followed by letters, digits, or the characters '.', ':', '-', and '_'" what


let explain_level = function
	| `Level1 -> 1
	| `Level2 -> 2
	| `Level3 -> 3


let explain_error = function

	| Error.Misplaced_label_parameter (tag, reason) ->
		let exp_reason = explain_reason "a" "label" reason
		in sprintf "Misplaced labelling for %s: %s." (explain_tag tag) exp_reason

	| Error.Misplaced_order_parameter (tag, reason) ->
		let exp_reason = explain_reason "an" "order" reason
		in sprintf "Misplaced ordering for %s: %s." (explain_tag tag) exp_reason

	| Error.Misplaced_extra_parameter (tag, reason) ->
		let exp_reason = explain_reason "an" "extra" reason
		in sprintf "Misplaced extra parameter for %s: %s." (explain_tag tag) exp_reason

	| Error.Invalid_label (tag, label) ->
		sprintf "Invalid label '%s' in %s. %s." label (explain_tag tag) (explain_ident "A label")

	| Error.Invalid_order_format (tag, order) ->
		sprintf "Unable to interpret the string '%s' in %s as an ordering." order (explain_tag tag)

	| Error.Invalid_order_levels (tag, order, expected, found) ->
		sprintf "Expected %d hierarchical levels in the ordering for %s, but the string '%s' contains %d instead." (explain_level expected) (explain_tag tag) order found

	| Error.Invalid_extra_boolean_parameter (tag, key, value) ->
		sprintf "In %s, the key '%s' expects a boolean parameter, yet the assigned value '%s' cannot be interpreted as such." (explain_tag tag) key value

	| Error.Invalid_extra_numeric_parameter (tag, key, value, low, high) ->
		sprintf "In %s, the key '%s' expects an integer x such that %d <= x <= %d, yet the assigned value '%s' cannot be interpreted as such." (explain_tag tag) key low high value

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
		sprintf "In %s, the extra parameter '%s' cannot be interpreted unambiguously." (explain_tag tag) extra

	| Error.Invalid_name_entity ent ->
		sprintf "Unknown entity '%s'." ent

	| Error.Invalid_deci_entity ent ->
		sprintf "Invalid Unicode decimal code point '%s'." ent

	| Error.Invalid_hexa_entity ent ->
		sprintf "Invalid Unicode hexadecimal code point '%s'." ent

	| Error.Invalid_macro_nargs (name, nargs) ->
		sprintf "Invalid number of parameters '%s' for macro '%s'.  Please provide an integer." nargs name

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

	| Error.Invalid_macro (tag, name) ->
		sprintf "Invalid macro name '%s' in %s. %s." name (explain_tag tag) (explain_ident "A macro")

	| Error.Duplicate_macro (tag, name) ->
		sprintf "The definition of macro '%s' in %s duplicates a previously defined macro." name (explain_tag tag)

	| Error.Undefined_macro (tag, name) ->
		sprintf "Reference to undefined macro '%s'.  Remember that macros must be defined before they are referenced and a macro may not invoke itself." name

	| Error.Invalid_custom (tag, env) ->
		sprintf "Invalid name '%s' for custom environment in %s. %s." env (explain_tag tag) (explain_ident "A custom environment")

	| Error.Duplicate_custom (tag, env) ->
		sprintf "The definition of custom environment '%s' in %s duplicates a previously defined environment." env (explain_tag tag)

	| Error.Undefined_custom (tag, env) ->
		sprintf "The environment '%s' used in %s has not been defined yet." env (explain_tag tag)

	| Error.Invalid_counter (tag, counter) ->
		sprintf "The counter '%s' requested in %s has been already assigned to a different class of custom environment." counter (explain_tag tag)

	| Error.Invalid_mathtex (tag, txt) ->
		sprintf "Invalid mathtex expression '%s' in %s." txt (explain_tag tag)

	| Error.Invalid_mathml (tag, txt) ->
		sprintf "Invalid mathml expression '%s' in %s." txt (explain_tag tag)

	| Error.Invalid_column_number (tag, orig_tag, orig_linenum, found, expected) ->
		sprintf "Wrong number of columns for a row declared by %s: found %d but expected %d columns.  This row belongs to the tabular environment declared in line %d by %s" (explain_tag tag) found expected orig_linenum (explain_tag orig_tag)

	| Error.Invalid_column_specifier (tag, spec) ->
		sprintf "Unknown column specifier '%s' in %s.  Valid column specifiers are c/C (for centred columns), l/L (for left aligned columns), r/R (for right aligned columns), and j/J (for justified columns)." spec (explain_tag tag)

	| Error.Invalid_cell_specifier (tag, spec) ->
		sprintf "Invalid cell specifier '%s' in %s.  Cell specifiers should consist of an integer indicating the span followed by a single character (either c/C, l/L, r/R, j/J) indicating the alignment." spec (explain_tag tag)

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
		sprintf "This line contains a malformed UTF-8 code point (represented by the character '\xef\xbf\xbd')."

	| Error.Reading_error msg ->
		sprintf "%s." msg

	| Error.Unavailable_feature (tag, description) ->
		sprintf "The feature '%s' requested by %s is unavailable for this document." description (explain_tag tag)

