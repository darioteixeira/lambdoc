(********************************************************************************)
(*	Implementation file for Write_xhtml module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Converts documents into XHTML.  The XHTML representation used is
	the one offered by Ocsigen's XHTML.M module.  This allows the direct
	use of the output of this module from within Ocsigen programmes.
*)

open Printf
open ExtList
open ExtString
open XHTML.M
open Basic


(********************************************************************************)
(**	{2 Exceptions}								*)
(********************************************************************************)

exception Command_see_with_non_note
exception Command_cite_with_non_bib
exception Command_ref_with_non_block
exception Command_sref_with_non_block
exception Empty_error_context
exception Empty_error_list


(********************************************************************************)
(**	{2 Private type definitions}						*)
(********************************************************************************)

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


let wrap_order order =
	match Order.string_of_order order with
		| ""	-> []
		| other	-> [span [pcdata other]]


let make_sref name ref order =
	make_internal_link (`User_label ref) [pcdata name; space (); pcdata (Order.string_of_order order)]


let make_sectional level label order content =
	let cons = match level with
		| Level.Level1 -> XHTML.M.h2
		| Level.Level2 -> XHTML.M.h3
		| Level.Level3 -> XHTML.M.h4
	in cons ?a:(Some [a_id (make_label label); a_class ["doc_sec"]]) ((wrap_order order) @ [span content])


let make_toc_entry label order content =
	XHTML.M.li [make_internal_link label ((wrap_order order) @ [span content])]


let make_align align =
	"doc_align_" ^ (Alignment.to_string align)


(********************************************************************************)
(**	{3 Conversion of valid documents}					*)
(********************************************************************************)

let write_valid_document settings classname doc =


	(************************************************************************)
	(* Converters for inline context.					*)
	(************************************************************************)

	let rec write_super_seq seq =
		List.map write_super_node seq


	and write_nonlink_seq seq =
		List.map write_nonlink_node seq


	and write_nonlink_node: Node.nonlink_node_t -> nonlink_node_xhtml_t = function
		| `Plain txt ->
			XHTML.M.pcdata txt 
		| `Entity txt ->
			XHTML.M.entity txt
		| `Math math ->
			XHTML.M.span ~a:[a_class ["doc_math"]] [Math.to_inline_xhtml math]
		| `Bold seq ->
			XHTML.M.b (write_super_seq seq)
		| `Emph seq ->
			XHTML.M.i (write_super_seq seq)
		| `Mono seq ->
			XHTML.M.span ~a:[a_class ["doc_mono"]] (write_super_seq seq)
		| `Caps seq ->
			XHTML.M.span ~a:[a_class ["doc_caps"]] (write_super_seq seq)
		| `Thru seq ->
			XHTML.M.span ~a:[a_class ["doc_thru"]] (write_super_seq seq)
		| `Sup seq ->
			XHTML.M.sup (write_super_seq seq)
		| `Sub seq ->
			XHTML.M.sub (write_super_seq seq)
		| `Box seq ->
			XHTML.M.span ~a:[a_class ["doc_box"]] (write_super_seq seq)


	and write_link_node : Node.link_node_t -> link_node_xhtml_t = function

		| `Link (lnk, seq) ->
			make_external_link lnk (write_nonlink_seq seq)

		| `See ref ->
			let order = Hashtbl.find doc.Valid.labels (`User_label ref)
			in (match order with
				| Order.Note_order o ->
					make_internal_link
						~classname:"doc_see"
						(`User_label ref)
						[pcdata (sprintf "(%s)" (Order.string_of_order o))]
				| _ ->
					raise Command_see_with_non_note)

		| `Cite ref ->
			let order = Hashtbl.find doc.Valid.labels (`User_label ref)
			in (match order with
				| Order.Bib_order o ->
					make_internal_link
						~classname:"doc_cite"
						(`User_label ref)
						[pcdata (sprintf "[%s]" (Order.string_of_order o))]
				| _ ->
					raise Command_cite_with_non_bib)

		| `Ref ref ->
			let order = Hashtbl.find doc.Valid.labels (`User_label ref)
			in (match order with
				| Order.Visible_order (Order.Body_sectional_order order) ->
					make_internal_link (`User_label ref) [pcdata (Order.string_of_order order)]
				| Order.Visible_order (Order.Appendix_sectional_order order) ->
					make_internal_link (`User_label ref) [pcdata (Order.string_of_order order)]
				| Order.Visible_order (Order.Preset_sectional_order order) ->
					make_internal_link (`User_label ref) [pcdata (Order.string_of_order order)]
				| Order.Visible_order (Order.Wrapper_order (_, order)) ->
					make_internal_link (`User_label ref) [pcdata (Order.string_of_order order)]
				| _ ->
					raise Command_ref_with_non_block)

		| `Sref ref ->
			let order = Hashtbl.find doc.Valid.labels (`User_label ref)
			in (match order with
				| Order.Visible_order (Order.Body_sectional_order order) ->
					make_sref settings.names.section_name ref order
				| Order.Visible_order (Order.Appendix_sectional_order order) ->
					make_sref settings.names.appendix_name ref order
				| Order.Visible_order (Order.Preset_sectional_order order) ->
					make_sref settings.names.section_name ref order
				| Order.Visible_order (Order.Wrapper_order (Order.Equation_wrapper, order)) ->
					make_sref settings.names.equation_name ref order
				| Order.Visible_order (Order.Wrapper_order (Order.Algorithm_wrapper, order)) ->
					make_sref settings.names.algorithm_name ref order
				| Order.Visible_order (Order.Wrapper_order (Order.Table_wrapper, order)) ->
					make_sref settings.names.table_name ref order
				| Order.Visible_order (Order.Wrapper_order (Order.Figure_wrapper, order)) ->
					make_sref settings.names.figure_name ref order
				| _ ->
					raise Command_sref_with_non_block)

		| `Mref (ref, seq) ->
			make_internal_link (`User_label ref) (write_nonlink_seq seq)


	and write_super_node = function
		| #Node.nonlink_node_t as node ->
			(write_nonlink_node node :> super_node_xhtml_t)
		| #Node.link_node_t as node->
			(write_link_node node :> super_node_xhtml_t) in


	(************************************************************************)
	(* Converters for tabular environment.					*)
	(************************************************************************)

	let write_tabular align tab =

		let ord = ref (-1) in

		let write_cell ord seq =
			let (alignment, weight) = Array.get tab.Tabular.tcols (ord+1) in
			let col_class = "doc_col_" ^ Tabular.alignment_to_string alignment
			in match weight with
				| Tabular.Normal -> XHTML.M.td ~a:[a_class [col_class]] (write_super_seq seq)
				| Tabular.Strong -> XHTML.M.th ~a:[a_class [col_class]] (write_super_seq seq) in

		let write_row (hd, tl) =
			incr ord;
			let row_class = "doc_row_" ^ (if !ord mod 2 = 0 then "even" else "odd")
			in XHTML.M.tr ~a:[a_class [row_class]] (write_cell (-1) hd) (List.mapi write_cell tl) in

		let write_group (hd, tl) =
			let hd = write_row hd in
			let tl = List.map write_row tl
			in (hd, tl) in

		let thead = match tab.Tabular.thead with
			| None		-> None
			| Some grp	-> let (hd, tl) = write_group grp in Some (XHTML.M.thead hd tl)

		and tfoot = match tab.Tabular.tfoot with
			| None		-> None
			| Some grp	-> let (hd, tl) = write_group grp in Some (XHTML.M.tfoot hd tl)

		and (tbody_hd, tbody_tl) =
			let write_tbody grp =
				let (hd, tl) = write_group grp
				in XHTML.M.tbody hd tl in
			let (hd, tl) = tab.Tabular.tbodies
			in (write_tbody hd, List.map write_tbody tl)

		in XHTML.M.tablex ~a:[a_class ["doc_tab"]] ?thead ?tfoot tbody_hd tbody_tl in


	(************************************************************************)
	(* Converters for document blocks.					*)
	(************************************************************************)

	let rec write_super_frag frag =
		List.map write_super_block frag


	and write_nestable_frag frag =
		List.map write_nestable_block frag


	and write_paragraph_frag frag =
		List.map write_paragraph_block frag


	and write_item_frag = function
		| (hd, tl) -> fplus (fun el -> XHTML.M.li (write_nestable_frag el)) hd tl


	and write_paragraph_block = function
		| `Paragraph seq ->
			XHTML.M.p ~a:[a_class ["doc_par"]] (write_super_seq seq)


	and write_itemize_block = function
		| `Itemize (bul, (hd_frag, tl_frags)) ->
			let (hd, tl) = write_item_frag (hd_frag, tl_frags)
			and style = "doc_style_" ^ (Bullet.to_string bul)
			in XHTML.M.ul ~a:[a_class ["doc_itemize"; style]] hd tl


	and write_enumerate_block = function
		| `Enumerate (num, (hd_frag, tl_frags)) ->
			let (hd, tl) = write_item_frag (hd_frag, tl_frags)
			and style = "doc_style_" ^ (Numbering.to_string num)
			in XHTML.M.ol ~a:[a_class ["doc_enumerate"; style]] hd tl


	and write_quote_block = function
		| `Quote (align, frag) ->
			XHTML.M.blockquote ~a:[a_class ["doc_quote"; make_align align]] (write_nestable_frag frag)


	and write_math_block = function
		| `Math (align, math) ->
			XHTML.M.div ~a:[a_class ["doc_math"; make_align align]] [Math.to_block_xhtml math]


	and write_code_block = function
		| `Code (align, highlight) ->
			Highlight.to_xhtml ~class_prefix:"doc_hl_" ~extra_classes:[make_align align] ~numbered:true ~zebra:true highlight


	and write_verbatim_block = function
		| `Verbatim (align, txt) ->
			XHTML.M.pre ~a:[a_class ["doc_verbatim"; make_align align]] [XHTML.M.pcdata txt]


	and write_tabular_block = function
		| `Tabular (align, tab) ->
			write_tabular align tab


	and write_image_block = function
		| `Image (align, alias) ->
			let image = XHTML.M.img ~src:(uri_of_string alias) ~alt:alias ()
			in XHTML.M.div ~a:[a_class ["doc_image"; make_align align]] [image]


	and write_subpage_block = function
		| `Subpage (align, frag) ->
			XHTML.M.div ~a:[a_class ["doc_subpage"; make_align align]] (write_super_frag frag)


	and write_equation_block = function
		| #Block.math_block_t as blk ->
			write_math_block blk


	and write_algorithm_block = function
		| #Block.code_block_t as blk ->
			write_code_block blk


	and write_table_block = function
		| #Block.tabular_block_t as blk ->
			write_tabular_block blk


	and write_figure_block = function
		| #Block.image_block_t as blk ->
			write_image_block blk
		| #Block.verbatim_block_t as blk ->
			write_verbatim_block blk
		| #Block.subpage_block_t as blk ->
			write_subpage_block blk


	and write_caption order wrapper_name caption =
		let caption_head = XHTML.M.h1 [pcdata (wrapper_name ^ "  " ^ (Order.string_of_order order) ^ ":")]
		and caption_body = XHTML.M.p (write_super_seq caption)
		in XHTML.M.div ~a:[a_class ["doc_caption"]] [caption_head; caption_body]


	and write_wrapper (label, order, caption) wrapper_classname wrapper_name wrapper_content =
		let caption_content = write_caption order wrapper_name caption in
		(*let style = "doc_align_" ^ (Alignment.to_string align) in*)
		let style = "" in
		let classnames = ["doc_wrapper"; wrapper_classname; style]
		in XHTML.M.div ~a:[a_id (make_label label); a_class classnames] [wrapper_content; caption_content]


	and write_nestable_block = function

		| #Block.paragraph_block_t as blk ->
			write_paragraph_block blk

		| #Block.itemize_block_t as blk ->
			write_itemize_block blk

		| #Block.enumerate_block_t as blk ->
			write_enumerate_block blk

		| #Block.quote_block_t as blk ->
			write_quote_block blk

		| #Block.math_block_t as blk ->
			write_math_block blk

		| #Block.code_block_t as blk ->
			write_code_block blk

		| #Block.verbatim_block_t as blk ->
			write_verbatim_block blk

		| #Block.tabular_block_t as blk ->
			write_tabular_block blk

		| #Block.image_block_t as blk ->
			write_image_block blk

		| #Block.subpage_block_t as blk ->
			write_subpage_block blk

		| `Equation (wrapper, equation) ->
			let wrapper_name = settings.names.equation_name
			and wrapper_content = write_equation_block equation
			in write_wrapper wrapper "doc_eq" wrapper_name wrapper_content

		| `Algorithm (wrapper, algorithm) ->
			let wrapper_name = settings.names.algorithm_name
			and wrapper_content = write_algorithm_block algorithm
			in write_wrapper wrapper "doc_alg" wrapper_name wrapper_content

		| `Table (wrapper, table) ->
			let wrapper_name = settings.names.table_name
			and wrapper_content = write_table_block table
			in write_wrapper wrapper "doc_table" wrapper_name wrapper_content

		| `Figure (wrapper, figure) ->
			let wrapper_name = settings.names.figure_name
			and wrapper_content = write_figure_block figure
			in write_wrapper wrapper "doc_fig" wrapper_name wrapper_content
	

	and write_heading_block = function

		| `Section (level, label, order, seq) ->
			make_sectional level label order (write_super_seq seq)

		| `Appendix (level, label, order, seq) ->
			make_sectional level label order (write_super_seq seq)

		| `Bibliography (label, order) ->
			let name = settings.names.bibliography_name in
			let title = [make_sectional Level.Level1 label order [XHTML.M.pcdata name]] in
			let bibs = match doc.Valid.bibs with
				| []	 -> []
				| hd::tl -> let (hd, tl) = fplus write_bib hd tl in [XHTML.M.ul ~a:[a_class ["doc_bibs"]] hd tl]
			in XHTML.M.div (title @ bibs)

		| `Notes (label, order) ->
			let name = settings.names.notes_name in
			let title = [make_sectional Level.Level1 label order [XHTML.M.pcdata name]] in
			let notes = match doc.Valid.notes with
				| []	 -> []
				| hd::tl -> let (hd, tl) = fplus write_note hd tl in [XHTML.M.ul ~a:[a_class ["doc_notes"]] hd tl]
			in XHTML.M.div (title @ notes)

		| `Toc (label, order) ->
			let name = settings.names.toc_name in
			let title = [make_sectional Level.Level1 label order [XHTML.M.pcdata name]] in
			let toc = match doc.Valid.toc with
				| []	 -> []
				| hd::tl -> let (hd, tl) = fplus write_toc_entry hd tl in [XHTML.M.ul ~a:[a_class ["doc_toc"]] hd tl]
			in XHTML.M.div (title @ toc)


	and write_top_block = function
		| `Heading heading ->
			write_heading_block heading
		| `Title seq ->
			XHTML.M.h1 ~a:[a_class ["doc_title"]] (write_super_seq seq)
		| `Subtitle seq ->
			XHTML.M.h2 ~a:[a_class ["doc_title"]] (write_super_seq seq)
		| `Abstract frag ->
			XHTML.M.div ~a:[a_class ["doc_abs"]] ((XHTML.M.h1 [pcdata "Abstract"]) :: (write_paragraph_frag frag))
		| `Part seq ->
			XHTML.M.h1 ~a:[a_class ["doc_part"]] (write_super_seq seq)
		| `Rule ->
			XHTML.M.hr ()


	and write_super_block = function
		| #Block.top_block_t as blk ->
			write_top_block blk
		| #Block.nestable_block_t as blk ->
			write_nestable_block blk


	(************************************************************************)
	(* Writers for ghost elements: notes, bib entries, and toc entries.	*)
	(************************************************************************)

	and write_note note =
		XHTML.M.li ~a:[a_class ["doc_note"]] (write_nestable_frag note.Note.content)


	and write_bib bib =
		XHTML.M.li
			[
			XHTML.M.p ~a:[a_class ["doc_bib_title"]] (write_super_seq bib.Bib.title);
			XHTML.M.p ~a:[a_class ["doc_bib_author"]] (write_super_seq bib.Bib.author);
			XHTML.M.p ~a:[a_class ["doc_bib_resource"]] (write_super_seq bib.Bib.resource)
			]


	and write_toc_entry = function
		| `Section (_, label, order, seq) ->
			make_toc_entry label order (write_super_seq seq)
		| `Appendix (_, label, order, seq) ->
			make_toc_entry label order (write_super_seq seq)
		| `Bibliography (label, order) ->
			make_toc_entry label order [pcdata settings.names.bibliography_name]
		| `Notes (label, order) ->
			make_toc_entry label order [pcdata settings.names.notes_name]
		| `Toc (label, order) ->
			make_toc_entry label order [pcdata settings.names.toc_name]


	in XHTML.M.div ~a:[a_class ["doc"; "doc_valid"; classname]] (write_super_frag doc.Valid.content)


(********************************************************************************)
(**	{3 Conversion of invalid documents}					*)
(********************************************************************************)

let write_error (error_context, error_msg) =

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

		| Error.Unknown_bullet (tag, bul) ->
			sprintf "Unknown bullet '%s' for command '%s'.  Valid bullet types are 'default', 'disc', 'circle', 'square', and 'none'." bul tag

		| Error.Unknown_numbering (tag, num) ->
			sprintf "Unknown numbering '%s' for command '%s'.  Valid numberings are 'default', 'decimal', 'roman', 'Roman', 'alpha', 'Alpha', and 'none'." num tag

		| Error.Unknown_alignment (tag, align) ->
			sprintf "Unknown alignment '%s' for command '%s'.  Valid alignments are 'center', 'left', and 'right'." align tag

		| Error.Unknown_language (tag, lang) ->
			sprintf "Unknown language '%s' for command '%s'.  Valid languages are 'c', 'ocaml', and 'pascal'." lang tag

		| Error.Unknown_env_command tag ->
			sprintf "Unknown command '\\begin{%s}.'" tag

		| Error.Unknown_simple_command tag ->
			sprintf "Unknown command '\\%s'." tag

		| Error.Duplicate_label (tag, label) ->
			sprintf "Command '%s' attempts to redefine label '%s'." tag label

		| Error.Invalid_column_specifier (tag, spec) ->
			sprintf "Unknown column specifier '%c' in command '%s'.  Valid column specifiers are c/C (for centred columns), l/L (for left aligned columns), r/R (for right aligned columns), and j/J (for justified columns)." spec tag

		| Error.Invalid_mathtex txt ->
			sprintf "Invalid mathtex expression '%s'." txt

		| Error.Invalid_mathml txt ->
			sprintf "Invalid mathml expression '%s'." txt

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

		| Error.Invalid_command_feature (comm, description) ->
			sprintf "The feature '%s' requested by command '%s' has been flagged as invalid for this document." description comm

		| Error.Invalid_operator_feature (op, description) ->
			sprintf "The feature '%s' requested by operator '%s' has been flagged as invalid for this document." description op

		| Error.Syntax_error ->
			"Syntax error"

	in XHTML.M.li ~a:[a_class ["doc_error"]]
		[
		XHTML.M.h1 [pcdata (sprintf "Error in line %d:" line_number)];
		show_context;
		XHTML.M.p [pcdata explain_msg]
		]


let write_invalid_document classname = function
	| []		-> raise Empty_error_list
	| hd::tl	-> div ~a:[a_class ["doc"; "doc_invalid"; classname]]
				[ul ~a:[a_class ["doc_errors"]] (write_error hd) (List.map write_error tl)]


(********************************************************************************)
(**	{3 Top-level conversion}						*)
(********************************************************************************)

let write_ambivalent_document settings classname = function
	| `Valid valid		-> write_valid_document settings classname valid
	| `Invalid invalid	-> write_invalid_document classname invalid


(********************************************************************************)
(**	{3 Classnames}								*)
(********************************************************************************)

let manuscript_classname = "doc_manuscript"

let composition_classname = "doc_composition"


(********************************************************************************)
(**	{2 Public types and functions}						*)
(********************************************************************************)

(**	The following types and functions conform to the Document_writer.S
	signature.
*)

type t = [ `Div ] XHTML.M.elt

let write_valid_manuscript ?(settings = Settings.default) doc =
	write_valid_document settings manuscript_classname doc

let write_valid_composition ?(settings = Settings.default) doc =
	write_valid_document settings composition_classname doc

let write_invalid_manuscript doc =
	write_invalid_document manuscript_classname doc

let write_invalid_composition doc =
	write_invalid_document composition_classname doc

let write_ambivalent_manuscript ?(settings = Settings.default) doc =
	write_ambivalent_document settings manuscript_classname doc

let write_ambivalent_composition ?(settings = Settings.default) doc =
	write_ambivalent_document settings composition_classname doc

