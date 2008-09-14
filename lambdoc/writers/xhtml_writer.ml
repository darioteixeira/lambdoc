(********************************************************************************)
(**	Converts documents into XHTML.  The XHTML representation used is
	the one offered by Ocsigen's XHTML.M module.  This allows the direct
	use of the output of this module from within Ocsigen programmes.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Printf
open ExtList
open ExtString
open XHTML.M
open Document_ref
open Document_node
open Document_tabular
open Document_block
open Document_ghost
open Document_error
open Document_settings
open Document_valid


(********************************************************************************)
(**	{2 Private types and exceptions}					*)
(********************************************************************************)

exception Command_see_with_non_note
exception Command_cite_with_non_bib
exception Command_ref_with_non_block
exception Command_sref_with_non_block
exception Empty_error_context
exception Empty_error_list

type textual_node_xhtml_t = [`PCDATA ] XHTML.M.elt
type nonlink_node_xhtml_t = [`B | `I | `PCDATA | `Span | `Sub | `Sup ] XHTML.M.elt
type link_node_xhtml_t = [`A ] XHTML.M.elt
type super_node_xhtml_t = [`A | `B | `I | `PCDATA | `Span | `Sub | `Sup ] XHTML.M.elt


(********************************************************************************)
(**	{2 Private functions}							*)
(********************************************************************************)


(********************************************************************************)
(**	{3 Helper functions}							*)
(********************************************************************************)

let alphaseq_of_int num =
	let base = 26 in
	let rec from_base10 num =
		let num = num -1
		in if num < base
		then
			[num]
		else
			let rem = num mod base
			and num = num / base
			in rem::(from_base10 num) in
	let alpha_of_int num =
		String.of_char (char_of_int (65 + num)) in
	let rems = from_base10 num in
	List.fold_left (^) "" (List.rev_map alpha_of_int rems)

let make_label = function
	| `Auto_label ref -> "doc:a:" ^ ref
	| `User_label ref -> "doc:u:" ^ ref

let make_link ?classname lnk content =
	let class_attr = match classname with
		| None		-> []
		| Some thing	-> [a_class [thing]] in
	let attr = [a_href (uri_of_string lnk)] @ class_attr
	in XHTML.M.a ~a:attr content

let make_external_link = make_link

let make_internal_link ?classname ref content = make_link ?classname ("#" ^ (make_label ref)) content

let rec dot_order = function
	| []		-> ""
	| [x]		-> x
	| hd::tl	-> hd ^ "." ^ (dot_order tl)

let make_appendic = function
	| []		-> []
	| hd::tl	-> (alphaseq_of_int hd) :: (List.map string_of_int tl)

let make_order = function
	| `Auto_order (Order.Numeric l)		-> dot_order (List.map string_of_int l)
	| `Auto_order (Order.Appendic l)	-> dot_order (make_appendic l)
	| `User_order l				-> l
	| `None_order				-> ""

let wrap_order order =
	match make_order order with
		| ""	-> []
		| other	-> [span [pcdata other]]

let make_sref name ref order =
	make_internal_link (`User_label ref) [pcdata name; space (); pcdata (make_order order)]

let make_sectional level label order content =
	XHTML.M.h1 ~a:[a_id (make_label label); a_class ["doc_sec"]] ((wrap_order order) @ [span content])

let make_toc_entry label order content =
	XHTML.M.li [make_internal_link label ((wrap_order order) @ [span content])]


(********************************************************************************)
(**	{3 Conversion of valid documents}					*)
(********************************************************************************)

let convert_valid_document classname doc =

	let rec convert_textual_node : Node.textual_node_t -> textual_node_xhtml_t = function
		| `Plain txt ->
			XHTML.M.pcdata txt 
		| `Entity txt ->
			XHTML.M.entity txt


	and convert_nonlink_node: Node.nonlink_node_t -> nonlink_node_xhtml_t = function

		| #Node.textual_node_t as node ->
			(convert_textual_node node :> nonlink_node_xhtml_t)
		| `Mathtex txt ->
			XHTML.M.span [pcdata txt]
		| `Mathml txt ->
			XHTML.M.span [pcdata txt]
		| `Bold seq ->
			XHTML.M.b (convert_super_seq seq)
		| `Emph seq ->
			XHTML.M.i (convert_super_seq seq)
		| `Mono seq ->
			XHTML.M.span ~a:[a_class ["doc_mono"]] (convert_super_seq seq)
		| `Caps seq ->
			XHTML.M.span ~a:[a_class ["doc_caps"]] (convert_super_seq seq)
		| `Thru seq ->
			XHTML.M.span ~a:[a_class ["doc_thru"]] (convert_super_seq seq)
		| `Sup seq ->
			XHTML.M.sup (convert_super_seq seq)
		| `Sub seq ->
			XHTML.M.sub (convert_super_seq seq)
		| `Box seq ->
			XHTML.M.span ~a:[a_class ["doc_box"]] (convert_super_seq seq)


	and convert_link_node : Node.link_node_t -> link_node_xhtml_t = function

		| `Link (lnk, seq) ->
			make_external_link lnk (convert_nonlink_seq seq)

		| `See ref ->
			let order = Hashtbl.find doc.Valid.labels (`User_label ref)
			in (match order with
				| Order.Note_order o ->
					make_internal_link
						~classname:"doc_see"
						(`User_label ref)
						[pcdata (sprintf "(%s)" (make_order o))]
				| _ ->
					raise Command_see_with_non_note)

		| `Cite ref ->
			let order = Hashtbl.find doc.Valid.labels (`User_label ref)
			in (match order with
				| Order.Bib_order o ->
					make_internal_link
						~classname:"doc_cite"
						(`User_label ref)
						[pcdata (sprintf "[%s]" (make_order o))]
				| _ ->
					raise Command_cite_with_non_bib)

		| `Ref ref ->
			let order = Hashtbl.find doc.Valid.labels (`User_label ref)
			in (match order with
				| Order.Block_order (Order.Body_sectional_order order) ->
					make_internal_link (`User_label ref) [pcdata (make_order order)]
				| Order.Block_order (Order.Appendix_sectional_order order) ->
					make_internal_link (`User_label ref) [pcdata (make_order order)]
				| Order.Block_order (Order.Preset_sectional_order order) ->
					make_internal_link (`User_label ref) [pcdata (make_order order)]
				| Order.Block_order (Order.Floater_order (_, order)) ->
					make_internal_link (`User_label ref) [pcdata (make_order order)]
				| _ ->
					raise Command_ref_with_non_block)

		| `Sref ref ->
			let order = Hashtbl.find doc.Valid.labels (`User_label ref)
			in (match order with
				| Order.Block_order (Order.Body_sectional_order order) ->
					let name = Settings.get_section_name doc.Valid.settings
					in make_sref name ref order
				| Order.Block_order (Order.Appendix_sectional_order order) ->
					let name = Settings.get_appendix_name doc.Valid.settings
					in make_sref name ref order
				| Order.Block_order (Order.Preset_sectional_order order) ->
					let name = Settings.get_section_name doc.Valid.settings
					in make_sref name ref order
				| Order.Block_order (Order.Floater_order (Order.Algorithm_floater, order)) ->
					let name = Settings.get_algorithm_name doc.Valid.settings
					in make_sref name ref order
				| Order.Block_order (Order.Floater_order (Order.Equation_floater, order)) ->
					let name = Settings.get_equation_name doc.Valid.settings
					in make_sref name ref order
				| Order.Block_order (Order.Floater_order (Order.Figure_floater, order)) ->
					let name = Settings.get_figure_name doc.Valid.settings
					in make_sref name ref order
				| Order.Block_order (Order.Floater_order (Order.Table_floater, order)) ->
					let name = Settings.get_table_name doc.Valid.settings
					in make_sref name ref order
				| _ ->
					raise Command_sref_with_non_block)

		| `Mref (ref, seq)
			-> make_internal_link (`User_label ref) (convert_nonlink_seq seq)


	and convert_super_node = function
		| #Node.nonlink_node_t as node ->
			(convert_nonlink_node node :> super_node_xhtml_t)
		| #Node.link_node_t as node->
			(convert_link_node node :> super_node_xhtml_t)


	and convert_textual_seq seq =
		List.map convert_textual_node seq


	and convert_super_seq seq =
		List.map convert_super_node seq


	and convert_nonlink_seq seq =
		List.map convert_nonlink_node seq


	and convert_tabular tab =

		let ord = ref (-1) in

		let convert_cell ord seq =
			let (alignment, weight) = Array.get tab.Tabular.tcols (ord+1) in
			let col_class = "doc_col_" ^ Tabular.alignment_to_string alignment
			in match weight with
				| Tabular.Normal -> XHTML.M.td ~a:[a_class [col_class]] (convert_super_seq seq)
				| Tabular.Strong -> XHTML.M.th ~a:[a_class [col_class]] (convert_super_seq seq) in

		let convert_row (hd, tl) =
			incr ord;
			let row_class = "doc_row_" ^ (if !ord mod 2 = 0 then "even" else "odd")
			in XHTML.M.tr ~a:[a_class [row_class]] (convert_cell (-1) hd) (List.mapi convert_cell tl) in

		let convert_group (hd, tl) =
			let hd = convert_row hd in
			let tl = List.map convert_row tl
			in (hd, tl) in

		let thead = match tab.Tabular.thead with
			| None		-> None
			| Some grp	-> let (hd, tl) = convert_group grp in Some (XHTML.M.thead hd tl)

		and tfoot = match tab.Tabular.tfoot with
			| None		-> None
			| Some grp	-> let (hd, tl) = convert_group grp in Some (XHTML.M.tfoot hd tl)

		and (tbody_hd, tbody_tl) =
			let convert_tbody grp =
				let (hd, tl) = convert_group grp
				in XHTML.M.tbody hd tl in
			let (hd, tl) = tab.Tabular.tbodies
			in (convert_tbody hd, List.map convert_tbody tl)

		in XHTML.M.tablex ~a:[a_class ["doc_tabular"]] ?thead ?tfoot tbody_hd tbody_tl


	and convert_heading = function

		| `Section (label, order, seq) ->
			make_sectional XHTML.M.h1 label order (convert_super_seq seq)

		| `Subsection (label, order, seq) ->
			make_sectional XHTML.M.h2 label order (convert_super_seq seq)

		| `Subsubsection (label, order, seq) ->
			make_sectional XHTML.M.h3 label order (convert_super_seq seq)

		| `Appendix (label, order, seq) ->
			make_sectional XHTML.M.h1 label order (convert_super_seq seq)

		| `Subappendix (label, order, seq) ->
			make_sectional XHTML.M.h2 label order (convert_super_seq seq)

		| `Subsubappendix (label, order, seq) ->
			make_sectional XHTML.M.h3 label order (convert_super_seq seq)

		| `Bibliography (label, order) ->
			let name = Settings.get_bibliography_name doc.Valid.settings in
			let title = [make_sectional XHTML.M.h1 label order [XHTML.M.pcdata name]]
			and bibs = match doc.Valid.bibs with
				| []	 -> []
				| hd::tl -> [XHTML.M.ul ~a:[a_class ["doc_bibs"]] (convert_bib hd) (List.map convert_bib tl)]
			in XHTML.M.div (title @ bibs)

		| `Notes (label, order) ->
			let name = Settings.get_notes_name doc.Valid.settings in
			let title = [make_sectional XHTML.M.h1 label order [XHTML.M.pcdata name]]
			and notes = match doc.Valid.notes with
				| []	 -> []
				| hd::tl -> [XHTML.M.ul ~a:[a_class ["doc_notes"]] (convert_note hd) (List.map convert_note tl)]
			in XHTML.M.div (title @ notes)

		| `Toc (label, order) ->
			let name = Settings.get_toc_name doc.Valid.settings in
			let title = [make_sectional XHTML.M.h1 label order [XHTML.M.pcdata name]]
			and toc = match doc.Valid.toc with
				| []	 -> []
				| hd::tl -> [XHTML.M.ul ~a:[a_class ["doc_toc"]] (convert_toc_entry hd) (List.map convert_toc_entry tl)]
			in XHTML.M.div (title @ toc)


	and convert_top_block = function
		| `Heading heading ->
			convert_heading heading
		| `Rule -> XHTML.M.hr ()


	and convert_nestable_block = function

		| `Paragraph seq ->
			XHTML.M.p ~a:[a_class ["doc_par"]] (convert_super_seq seq)

		| `Mathtex txt ->
			XHTML.M.p [pcdata txt]

		| `Mathml txt ->
			XHTML.M.p [pcdata txt]

		| `Tabular tab ->
			convert_tabular tab

		| `Preformat seq ->
			XHTML.M.pre ~a:[a_class ["doc_pre"]] (convert_textual_seq seq)

		| `Itemize (bul, (head_frag, tail_frags)) ->
			let hd = XHTML.M.li (convert_nestable_frag head_frag)
			and tl = List.map (fun frag -> XHTML.M.li (convert_nestable_frag frag)) tail_frags
			and style = "doc_style_" ^ (Bullet.to_string bul)
			in XHTML.M.ul ~a:[a_class ["doc_itemize"; style]] hd tl

		| `Enumerate (num, (head_frag, tail_frags)) ->
			let hd = XHTML.M.li (convert_nestable_frag head_frag)
			and tl = List.map (fun frag -> XHTML.M.li (convert_nestable_frag frag)) tail_frags
			and style = "doc_style_" ^ (Numbering.to_string num)
			in XHTML.M.ol ~a:[a_class ["doc_enumerate"; style]] hd tl

		| `Quote (align, frag) ->
			let style = "doc_align_" ^ (Alignment.to_string align)
			in XHTML.M.blockquote ~a:[a_class ["doc_quote"; style]] (convert_nestable_frag frag)

		| `Algorithm (align, label, order, maybe_caption, text, syntax) ->
			let floater_content = XHTML.M.pre (convert_textual_seq text)
			and floater_name = Settings.get_algorithm_name doc.Valid.settings
			in convert_floater align label order maybe_caption "doc_alg" floater_name floater_content

		| `Equation (align, label, order, maybe_caption, text) ->
			let floater_content = XHTML.M.pre [pcdata text]
			and floater_name = Settings.get_equation_name doc.Valid.settings
			in convert_floater align label order maybe_caption "doc_eq" floater_name floater_content

		| `Figure (align, label, order, maybe_caption, fig) ->
			let floater_name = Settings.get_figure_name doc.Valid.settings
			and floater_content = match fig with
				| `Bitmap name ->
					XHTML.M.div [XHTML.M.img ~a:[a_class ["doc_fig_bitmap"]] ~src:(uri_of_string name) ~alt:name ()]
				| `Vector name ->
					XHTML.M.div [XHTML.M.img ~a:[a_class ["doc_fig_vector"]] ~src:(uri_of_string name) ~alt:name ()]
				| `Ascii text ->
					XHTML.M.pre ~a:[a_class ["doc_fig_ascii"]] (convert_textual_seq text)
				| `Subpage frag ->
					XHTML.M.div ~a:[a_class ["doc_fig_subpage"]] (convert_super_frag frag)
			in convert_floater align label order maybe_caption "doc_fig" floater_name floater_content
	
		| `Table (align, label, order, maybe_caption, tab) ->
			let floater_content = convert_tabular tab
			and floater_name = Settings.get_table_name doc.Valid.settings
			in convert_floater align label order maybe_caption "doc_tab" floater_name floater_content


	and convert_super_block = function
		| #Block.top_block_t as blk		-> convert_top_block blk
		| #Block.nestable_block_t as blk	-> convert_nestable_block blk


	and convert_nestable_frag frag =
		List.map convert_nestable_block frag


	and convert_super_frag frag =
		List.map convert_super_block frag


	and convert_floater align label order maybe_caption floater_classname floater_name floater_content =
		let (captioned_name, caption_content) = convert_caption order floater_name maybe_caption in
		let style = "doc_align_" ^ (Alignment.to_string align) in
		let classnames = ["doc_floater"; floater_classname; captioned_name; style]
		in XHTML.M.div ~a:[a_id (make_label label); a_class classnames] ([floater_content] @ caption_content)


	and convert_caption order floater_name = function
		| None ->
			("doc_uncaptioned", [])
		| Some caption	->
			let caption_head = XHTML.M.h1 [pcdata (floater_name ^ "  " ^ (make_order order) ^ ":")]
			and caption_body = XHTML.M.p (convert_super_seq caption)
			in ("doc_captioned", [XHTML.M.div ~a:[a_class ["doc_caption"]] [caption_head; caption_body]])


	and convert_toc_entry = function
		| `Section (label, order, seq)
		| `Subsection (label, order, seq)
		| `Subsubsection (label, order, seq) ->
			make_toc_entry label order (convert_super_seq seq)
		| `Appendix (label, order, seq)
		| `Subappendix (label, order, seq)
		| `Subsubappendix (label, order, seq) ->
			make_toc_entry label order (convert_super_seq seq)
		| `Bibliography (label, order) ->
			let name = Settings.get_bibliography_name doc.Valid.settings
			in make_toc_entry label order [pcdata name]
		| `Notes (label, order) ->
			let name = Settings.get_notes_name doc.Valid.settings
			in make_toc_entry label order [pcdata name]
		| `Toc (label, order) ->
			let name = Settings.get_toc_name doc.Valid.settings
			in make_toc_entry label order [pcdata name]

	and convert_bib bib =
		XHTML.M.li
			[
			XHTML.M.p ~a:[a_class ["doc_bib_title"]] (convert_super_seq bib.Bib.title);
			XHTML.M.p ~a:[a_class ["doc_bib_author"]] (convert_super_seq bib.Bib.author);
			XHTML.M.p ~a:[a_class ["doc_bib_resource"]] (convert_super_seq bib.Bib.resource)
			]

	and convert_note note =
		XHTML.M.li ~a:[a_class ["doc_note"]] (convert_super_seq note.Note.content)

	in div ~a:[a_class ["doc"; "doc_valid"; classname]] (convert_super_frag doc.Valid.content)


(********************************************************************************)
(**	{3 Conversion of invalid documents}					*)
(********************************************************************************)

let convert_error (error_context, error_msg) =

	let line_number = error_context.Error.error_line_number
	and line_before = error_context.Error.error_line_before
	and line_actual = error_context.Error.error_line_actual
	and line_after = error_context.Error.error_line_after in

	let show_context =
		let show_line classname delta line =
			XHTML.M.li ~a:[a_class [classname]]
				[
				span [pcdata (sprintf "%03d" (line_number + delta))];
				span [pcdata line]
				] in
		let show_line_around delta line =
			show_line "doc_error_around" delta line
		and show_line_actual =
			show_line "doc_error_actual" 0 line_actual in
		let lines =
			(List.mapi (fun ord line -> show_line_around (ord - (List.length line_before)) line) line_before) @
			[show_line_actual] @
			(List.mapi (fun ord line -> show_line_around (ord+1) line) line_after)
		in match lines with
			| []		-> raise Empty_error_context
			| hd::tl	-> XHTML.M.ul ~a:[a_class ["doc_error_lines"]] hd tl in

	let explain_reason article what = function
		| Error.Reason_is_empty_when_non_empty_mandatory ->
			sprintf "you provided an empty %s parameter, but it should contain something" what
		| Error.Reason_is_empty_when_forbidden ->
			sprintf "you provided an empty %s parameter, but it is altogether forbidden for this command" what
		| Error.Reason_is_non_empty_when_forbidden str ->
			sprintf "you provided %s %s parameter '%s', but this command forbids it" article what str
		| Error.Reason_is_absent_when_mandatory ->
			sprintf "you have not provided %s %s parameter, but it is mandatory for this command" article what in

	let explain_msg = match error_msg with

		| Error.Bad_label_parameter (tag, reason) ->
			let exp_reason = explain_reason "a" "label" reason
			in sprintf "Invalid labelling for command '%s': %s." tag exp_reason

		| Error.Bad_order_parameter (tag, reason) ->
			let exp_reason = explain_reason "an" "order" reason
			in sprintf "Invalid ordering for command '%s': %s." tag exp_reason

		| Error.Bad_extra_parameter (tag, reason) ->
			let exp_reason = explain_reason "an" "extra" reason
			in sprintf "Invalid extra parameter for command '%s': %s." tag exp_reason

		| Error.Bad_secondary_parameter (tag, reason) ->
			let exp_reason = explain_reason "a" "secondary" reason
			in sprintf "Invalid secondary parameter for command '%s': %s." tag exp_reason

		| Error.Unknown_bullet_type (tag, bul) ->
			sprintf "Unknown bullet type '%s' for command '%s'.  Valid bullet types are 'default', 'disc', 'circle', 'square', and 'none'." bul tag

		| Error.Unknown_numbering_type (tag, num) ->
			sprintf "Unknown numbering type '%s' for command '%s'.  Valid numberings are 'default', 'decimal', 'roman', 'Roman', 'alpha', 'Alpha', and 'none'." num tag

		| Error.Unknown_alignment_type (tag, num) ->
			sprintf "Unknown alignment type '%s' for command '%s'.  Valid alignments are 'center', 'left', and 'right'." num tag

		| Error.Unknown_figure_type (tag, fig) ->
			sprintf "Unknown figure type '%s' for command '%s'.  Valid figure types are 'bitmap', 'vector', 'ascii', and 'subpage'." fig tag

		| Error.Unknown_setting setting ->
			sprintf "Unknown setting '%s'." setting

		| Error.Unknown_env_command tag ->
			sprintf "Unknown command '\\begin{%s}.'" tag

		| Error.Unknown_simple_command tag ->
			sprintf "Unknown command '\\%s'." tag

		| Error.Duplicate_label (tag, label) ->
			sprintf "Command '%s' attempts to redefine label '%s'." tag label

		| Error.Duplicate_block (tag) ->
			sprintf "Command '%s' duplicates an already defined block." tag

		| Error.Missing_block (tag, blk) ->
			sprintf "Command '%s' is missing a required definition of block '%s'." tag blk

		| Error.Invalid_block (tag, blk) ->
			sprintf "Block '%s' is invalid for command '%s'." blk tag

		| Error.Invalid_column_specifier (tag, spec) ->
			sprintf "Unknown column specifier '%c' in command '%s'.  Valid column specifiers are c/C (for centred columns), l/L (for left aligned columns), r/R (for right aligned columns), and j/J (for justified columns)." spec tag

		| Error.Wrong_column_number (main_linenum, found, expected) ->
			sprintf "Wrong number of columns for a row belonging to the tabular environment started in line %d: found %d but expected %d columns." main_linenum found expected

		| Error.Empty_target (tag, label) ->
			sprintf "Empty target for command '%s' and label '%s'." tag label

		| Error.Wrong_target (tag, expected, suggested, label) ->
			let str_expected = match expected with
				| Error.Target_bib    -> "bibliography notes"
				| Error.Target_note   -> "note definitions"
				| Error.Target_label  -> "document labels"
			and str_suggested = match suggested with
				| Error.Target_bib    -> "'\\cite'"
				| Error.Target_note   -> "'\\see'"
				| Error.Target_label  -> "'\\ref', '\\sref', or '\\mref'"
			in sprintf ("Wrong target '%s' for command '%s': this command should only be used to reference %s.  Considering your target, perhaps you mean to use command %s instead?") label tag str_expected str_suggested

		| Error.Absent_target (tag, label) ->
			sprintf "Command '%s' references an undefined label '%s'." tag label

		| Error.Invalid_composition_subset tag ->
			sprintf "Command '%s' is not valid for compositions." tag

		| Error.Syntax_error ->
			"Syntax error"

	in XHTML.M.li ~a:[a_class ["doc_error"]]
		[
		XHTML.M.h1 [pcdata (sprintf "Error in line %d:" line_number)];
		show_context;
		XHTML.M.p [pcdata explain_msg]
		]


let convert_invalid_document classname = function
	| []		-> raise Empty_error_list
	| hd::tl	-> div ~a:[a_class ["doc"; "doc_invalid"; classname]]
				[ul ~a:[a_class ["doc_errors"]] (convert_error hd) (List.map convert_error tl)]


(********************************************************************************)
(**	{3 Top-level conversion}						*)
(********************************************************************************)

let convert_ambivalent_document classname = function
	| `Valid valid		-> convert_valid_document classname valid
	| `Invalid invalid	-> convert_invalid_document classname invalid


(********************************************************************************)
(**	{3 Classnames}								*)
(********************************************************************************)

let manuscript_classname = "doc_manuscript"

let composition_classname = "doc_composition"


(********************************************************************************)
(**	{2 Public functions}							*)
(********************************************************************************)

let ambivalent_manuscript_to_xhtml = convert_ambivalent_document manuscript_classname
let valid_manuscript_to_xhtml = convert_valid_document manuscript_classname
let invalid_manuscript_to_xhtml = convert_invalid_document manuscript_classname

let ambivalent_composition_to_xhtml = convert_ambivalent_document composition_classname
let valid_composition_to_xhtml = convert_valid_document composition_classname
let invalid_composition_to_xhtml = convert_invalid_document composition_classname

