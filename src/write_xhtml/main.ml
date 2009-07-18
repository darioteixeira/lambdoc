(********************************************************************************)
(*	Main.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
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

open Lambdoc_core
open Lambdoc_writer
open Basic
open Valid
open Settings


(********************************************************************************)
(**	{2 Exceptions}								*)
(********************************************************************************)

exception Command_see_with_non_note of Target.t
exception Command_cite_with_non_bib of Target.t
exception Command_ref_with_non_visible_block of Target.t
exception Command_sref_with_non_visible_block of Target.t
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


let wrap_order = function
	| ""	-> []
	| order	-> [span [pcdata order]]


let cons_of_level = function
	| `Level1 -> XHTML.M.h1
	| `Level2 -> XHTML.M.h2
	| `Level3 -> XHTML.M.h3


let class_of_level = function
	| `Level0 -> "level0"
	| `Level1 -> "level1"
	| `Level2 -> "level2"
	| `Level3 -> "level3"


let make_alignment alignment = ["doc_aligned"; "doc_align_" ^ (Alignment.to_string alignment)]
	

let make_heading cons label order_str classname content =
	cons ?a:(Some [a_id (make_label label); a_class [classname]]) ((wrap_order order_str) @ [span content])


let make_sectional level label order_str content =
	make_heading (cons_of_level level) label order_str "doc_sec" content


(********************************************************************************)
(**	{3 Converters}								*)
(********************************************************************************)

let part_conv order = Order.string_of_ordinal Printers.roman order


let section_conv location order =
	let conv = match location with
		| `Mainbody	-> Printers.mainbody
		| `Appendixed	-> Printers.appendixed
	in Order.string_of_hierarchical conv order


let wrapper_conv order = Order.string_of_ordinal Printers.arabic order


let bib_conv order = Order.string_of_ordinal Printers.arabic order


let note_conv order = Order.string_of_ordinal Printers.arabic order


(********************************************************************************)
(**	{3 Conversion of valid documents}					*)
(********************************************************************************)

let write_valid_document settings classname doc =

	(************************************************************************)
	(* Fetch fields from doc record.					*)
	(************************************************************************)

	let content = doc.Valid.content
	and bibs = doc.Valid.bibs
	and notes = doc.Valid.notes
	and toc = doc.Valid.toc
	and labelmap = doc.Valid.labelmap in


	(************************************************************************)
	(* Converters for inline context.					*)
	(************************************************************************)

	let rec write_seq ?(nbspfy=false) seq =
		List.map (write_inline ~nbspfy) seq


	and write_inline ~nbspfy = function

		| `Plain txt ->
			let to_nbsp txt =
				let rec trans = function
					| hd1 :: hd2 :: tl	-> (XHTML.M.pcdata hd1) :: (XHTML.M.space ()) :: (trans (hd2 :: tl))
					| hd :: tl		-> (XHTML.M.pcdata hd) :: (trans tl)
					| []			-> []
				in trans (String.nsplit txt " ")
			in if nbspfy
			then XHTML.M.span (to_nbsp txt)
			else XHTML.M.pcdata txt

		| `Entity num ->
			XHTML.M.entity ("#" ^ (string_of_int num))

		| `Math math ->
			XHTML.M.span ~a:[a_class ["doc_math"]] [Math.to_inline_xhtml math]

		| `Bold seq ->
			XHTML.M.b (write_seq ~nbspfy seq)

		| `Emph seq ->
			XHTML.M.i (write_seq ~nbspfy seq)

		| `Mono seq ->
			XHTML.M.span ~a:[a_class ["doc_mono"]] (write_seq ~nbspfy seq)

		| `Caps seq ->
			XHTML.M.span ~a:[a_class ["doc_caps"]] (write_seq ~nbspfy seq)

		| `Thru seq ->
			XHTML.M.span ~a:[a_class ["doc_thru"]] (write_seq ~nbspfy seq)

		| `Sup seq ->
			XHTML.M.sup (write_seq ~nbspfy seq)

		| `Sub seq ->
			XHTML.M.sub (write_seq ~nbspfy seq)

		| `Mbox seq ->
			XHTML.M.span (write_seq ~nbspfy:true seq)

		| `Link (lnk, seq) ->
			make_external_link lnk (Obj.magic (write_seq ~nbspfy seq))

		| `See ref ->
			let label = `User_label ref in
			let target = Labelmap.find labelmap label
			in (match target with
				| Target.Note_target order ->
					let content = [pcdata (sprintf "(%s)" (note_conv order))]
					in make_internal_link ~classname:"doc_see" label content
				| _ ->
					raise (Command_see_with_non_note target))

		| `Cite ref ->
			let label = `User_label ref in
			let target = Labelmap.find labelmap label
			in (match target with
				| Target.Bib_target order ->
					let content = [pcdata (sprintf "[%s]" (bib_conv order))]
					in make_internal_link ~classname:"doc_cite" label content
				| _ ->
					raise (Command_cite_with_non_bib target))

		| `Ref ref ->
			let label = `User_label ref in
			let target = Labelmap.find labelmap label
			in (match target with
				| Target.Visible_target (Target.Part_target order) ->
					let content = [pcdata (part_conv order)]
					in make_internal_link label content
				| Target.Visible_target (Target.Section_target (location, order)) ->
					let content = [pcdata (section_conv location order)]
					in make_internal_link label content
				| Target.Visible_target (Target.Wrapper_target (_, order)) ->
					let content = [pcdata (wrapper_conv order)]
					in make_internal_link label content
				| _ ->
					raise (Command_ref_with_non_visible_block target))

		| `Sref ref ->
			let target = Labelmap.find labelmap (`User_label ref) in
			let make_sref name order_str = make_internal_link (`User_label ref) [pcdata name; space (); pcdata order_str]
			in (match target with
				| Target.Visible_target (Target.Part_target order) ->
					make_sref settings.names.part_name (part_conv order)
				| Target.Visible_target (Target.Section_target (location, order)) ->
					let name = match location with
						| `Mainbody	-> settings.names.section_name
						| `Appendixed	-> settings.names.appendix_name
					in make_sref name (section_conv location order)
				| Target.Visible_target (Target.Wrapper_target (Target.Equation_wrapper, order)) ->
					make_sref settings.names.equation_name (wrapper_conv order)
				| Target.Visible_target (Target.Wrapper_target (Target.Printout_wrapper, order)) ->
					make_sref settings.names.printout_name (wrapper_conv order)
				| Target.Visible_target (Target.Wrapper_target (Target.Table_wrapper, order)) ->
					make_sref settings.names.table_name (wrapper_conv order)
				| Target.Visible_target (Target.Wrapper_target (Target.Figure_wrapper, order)) ->
					make_sref settings.names.figure_name (wrapper_conv order)
				| _ ->
					raise (Command_sref_with_non_visible_block target))

		| `Mref (ref, seq) ->
			make_internal_link (`User_label ref) (Obj.magic (write_seq ~nbspfy seq)) in


	(************************************************************************)
	(* Converters for tabular environment.					*)
	(************************************************************************)

	let write_tabular style tab =

		let ord = ref (-1) in

		let write_cell ord seq =
			let (alignment, weight) = Array.get tab.Tabular.tcols (ord+1) in
			let col_class = "doc_col_" ^ Tabular.alignment_to_string alignment
			in match weight with
				| Tabular.Normal -> XHTML.M.td ~a:[a_class [col_class]] (write_seq seq)
				| Tabular.Strong -> XHTML.M.th ~a:[a_class [col_class]] (write_seq seq) in

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

		and (tbody_hd, tbody_tl) =
			let write_tbody grp =
				let (hd, tl) = write_group grp
				in XHTML.M.tbody hd tl in
			let (hd, tl) = tab.Tabular.tbodies
			in (write_tbody hd, List.map write_tbody tl)

		and tfoot = match tab.Tabular.tfoot with
			| None		-> None
			| Some grp	-> let (hd, tl) = write_group grp in Some (XHTML.M.tfoot hd tl)

		in XHTML.M.div ~a:[a_class (["doc_tab"] @ style)] [XHTML.M.div [XHTML.M.tablex ?thead ?tfoot tbody_hd tbody_tl]] in


	(************************************************************************)
	(* Converters for document blocks.					*)
	(************************************************************************)

	let rec write_frag frag =
		(*List.map (fun (_, blk) -> blk) (List.map (write_block ~wrapped:false) frag)*)
		List.map (fun blk -> let (_, res) = write_block ~wrapped:false blk in res) frag


	and write_block ?(wrapped = false) = function

		| `Paragraph seq ->
			(None, XHTML.M.p ~a:[a_class ["doc_par"]] (write_seq seq))

		| `Itemize (bul, (hd_frag, tl_frags)) ->
			let (hd, tl) = fplus (fun frag -> XHTML.M.li (write_frag frag)) hd_frag tl_frags
			and style = "doc_style_" ^ (Bullet.to_string bul)
			in (None, XHTML.M.ul ~a:[a_class ["doc_itemize"; style]] hd tl)

		| `Enumerate (num, (hd_frag, tl_frags)) ->
			let (hd, tl) = fplus (fun frag -> XHTML.M.li (write_frag frag)) hd_frag tl_frags
			and style = "doc_style_" ^ (Numbering.to_string num)
			in (None, XHTML.M.ol ~a:[a_class ["doc_enumerate"; style]] hd tl)

		| `Description (hd_frag, tl_frags) ->
			let write_describe_frag (seq, frag) = (XHTML.M.dt (write_seq seq), XHTML.M.dd (write_frag frag)) in
			let (hd, tl) = fplus write_describe_frag hd_frag tl_frags in
			let (new_hd, new_tl) =
				let (first, second) = hd
				in (first, second :: (List.flatten (List.map (fun (x, y) -> [x; y]) tl)))
			in (None, XHTML.M.dl ~a:[a_class ["doc_description"]] new_hd new_tl)

		| `Quote (alignment, frag) ->
			let style = if wrapped then [] else make_alignment alignment
			in (None, XHTML.M.blockquote ~a:[a_class (["doc_quote"] @ style)] (write_frag frag))

		| `Callout (alignment, maybe_classname, maybe_seq, frag) ->
			let style_align = if wrapped then [] else make_alignment alignment
			and style_class = match maybe_classname with Some classname -> ["doc_callout_" ^ classname] | None -> [] in
			let title = match maybe_seq with
				| None -> []
				| Some seq -> [XHTML.M.div ~a:[a_class ["doc_callout_head"]] [XHTML.M.h1 (write_seq seq)]]
			in (None, XHTML.M.div ~a:[a_class (["doc_callout"] @ style_align @ style_class)]
				[XHTML.M.div (title @ [XHTML.M.div ~a:[a_class ["doc_callout_body"]] (write_frag frag)])])

		| `Math (alignment, math) ->
			let style = if wrapped then [] else make_alignment alignment
			in (Some alignment, XHTML.M.div ~a:[a_class (["doc_math"] @ style)] [Math.to_block_xhtml math])

		| `Code (alignment, linenums, zebra, code) ->
			let style = if wrapped then [] else make_alignment alignment
			in (Some alignment, Highlight.to_xhtml ~class_prefix:"doc_hl_" ~extra_classes:style ~linenums ~zebra code)

		| `Tabular (alignment, tab) ->
			let style = if wrapped then [] else make_alignment alignment
			in (Some alignment, write_tabular style tab)

		| `Verbatim (alignment, txt) ->
			let style = if wrapped then [] else make_alignment alignment
			in (Some alignment, XHTML.M.div ~a:[a_class (["doc_verb"] @ style)] [XHTML.M.div [XHTML.M.pre [XHTML.M.pcdata txt]]])

		| `Bitmap (alignment, (shadow, width, alias, alt)) ->
			let style_align = if wrapped then [] else make_alignment alignment
			and style_shadow = if shadow then ["doc_bitmap_shadow"] else [] in
			let style = style_align @ style_shadow in
			let attrs = match width with
				| Some w	-> [a_width (`Percent w)]
				| None		-> [] in
			let uri = uri_of_string alias in
			let bitmap = XHTML.M.a ~a:[a_href uri] [XHTML.M.img ~a:attrs ~src:uri ~alt ()]
			in (Some alignment, XHTML.M.div ~a:[a_class (["doc_bitmap"] @ style)] [bitmap])

		| `Subpage (alignment, frag) ->
			let style = if wrapped then [] else make_alignment alignment
			in (Some alignment, XHTML.M.div ~a:[a_class (["doc_subpage"] @ style)] [XHTML.M.div (write_frag frag)])

		| `Equation (wrapper, equation) ->
			let name = settings.names.equation_name
			and content = write_block ~wrapped:true equation
			in (None, write_wrapper wrapper "doc_eq" name content)

		| `Printout (wrapper, printout) ->
			let name = settings.names.printout_name
			and content = write_block ~wrapped:true printout
			in (None, write_wrapper wrapper "doc_prt" name content)

		| `Table (wrapper, table) ->
			let name = settings.names.table_name
			and content = write_block ~wrapped:true table
			in (None, write_wrapper wrapper "doc_table" name content)

		| `Figure (wrapper, figure) ->
			let name = settings.names.figure_name
			and content = write_block ~wrapped:true figure
			in (None, write_wrapper wrapper "doc_fig" name content)

		| `Heading heading ->
			(None, write_heading_block heading)

		| `Title (level, seq) ->
			(None, (cons_of_level level) ~a:[a_class ["doc_title"]] (write_seq seq))

		| `Abstract frag ->
			(None, XHTML.M.div ~a:[a_class ["doc_abs"]] ((XHTML.M.h1 [pcdata "Abstract"]) :: (write_frag frag)))

		| `Rule ->
			(None, XHTML.M.hr ())


	and write_heading_block = function

		| `Part (label, order, `Custom seq) ->
			make_heading XHTML.M.h1 label (part_conv order) "doc_part" (write_seq seq)

		| `Part (label, order, `Appendix) ->
			make_heading XHTML.M.h1 label (part_conv order) "doc_part" [XHTML.M.pcdata settings.names.appendix_name]

		| `Section (label, order, location, level, `Custom seq) ->
			make_sectional level label (section_conv location order) (write_seq seq)

		| `Section (label, order, location, level, `Bibliography) ->
			let name = settings.names.bibliography_name in
			let title = [make_sectional level label (section_conv location order) [XHTML.M.pcdata name]] in
			let bibs = match bibs with
				| []	 -> []
				| hd::tl -> let (hd, tl) = fplus write_bib hd tl in [XHTML.M.ol ~a:[a_class ["doc_bibs"]] hd tl]
			in XHTML.M.div (title @ bibs)

		| `Section (label, order, location, level, `Notes) ->
			let name = settings.names.notes_name in
			let title = [make_sectional level label (section_conv location order) [XHTML.M.pcdata name]] in
			let notes = match notes with
				| []	 -> []
				| hd::tl -> let (hd, tl) = fplus write_note hd tl in [XHTML.M.ol ~a:[a_class ["doc_notes"]] hd tl]
			in XHTML.M.div (title @ notes)

		| `Section (label, order, location, level, `Toc) ->
			let name = settings.names.toc_name in
			let title = [make_sectional level label (section_conv location order) [XHTML.M.pcdata name]] in
			let toc = match toc with
				| []	 -> []
				| hd::tl -> let (hd, tl) = fplus write_toc_entry hd tl in [XHTML.M.ul ~a:[a_class ["doc_toc"]] hd tl]
			in XHTML.M.div (title @ toc)


	and write_wrapper (label, order, seq) classname wrapper_name (maybe_alignment, wrapper_content) =
		let alignment = match maybe_alignment with
			| Some alignment -> alignment
			| None		 -> failwith "write_wrapper" in
		let caption_content =
			let caption_head = XHTML.M.span [pcdata wrapper_name; entity "thinsp"; pcdata ((wrapper_conv order) ^ ":")]
			and caption_body = XHTML.M.span (write_seq seq)
			in XHTML.M.p ~a:[a_class ["doc_caption"]] [caption_head; caption_body] in
		let classnames = ["doc_wrapper"; classname] @ (make_alignment alignment)
		in XHTML.M.div ~a:[a_id (make_label label); a_class classnames] [wrapper_content; caption_content]


	(************************************************************************)
	(* Writers for ghost elements: notes, bib entries, and TOC entries.	*)
	(************************************************************************)

	and write_note note =
		XHTML.M.li ~a:[a_id (make_label note.Note.label)]
			[
			XHTML.M.span [pcdata ("(" ^ (note_conv note.Note.order) ^ ")")];
			XHTML.M.div (write_frag note.Note.content);
			]


	and write_bib bib =
		XHTML.M.li ~a:[a_id (make_label bib.Bib.label)]
			[
			XHTML.M.span [pcdata ("[" ^ (bib_conv bib.Bib.order) ^ "]")];
			XHTML.M.p
				[
				XHTML.M.span ~a:[a_class ["doc_bib_author"]] (write_seq bib.Bib.author);
				XHTML.M.span ~a:[a_class ["doc_bib_title"]] (write_seq bib.Bib.title);
				XHTML.M.span ~a:[a_class ["doc_bib_resource"]] (write_seq bib.Bib.resource);
				]
			]


	and write_toc_entry sec =
		let make_toc_entry label classname order_str content =
		        XHTML.M.li ~a:[a_class [classname]] [make_internal_link label ((wrap_order order_str) @ [span content])]
		in match sec with
			| `Part (label, order, `Custom seq) ->
				make_toc_entry label (class_of_level `Level0) (part_conv order) (write_seq seq)
			| `Part (label, order, `Appendix) ->
				make_toc_entry label (class_of_level `Level0) (part_conv order) [pcdata settings.names.appendix_name]
			| `Section (label, order, location, level, `Custom seq) ->
				make_toc_entry label (class_of_level level) (section_conv location order) (write_seq seq)
			| `Section (label, order, location, level, `Bibliography) ->
				make_toc_entry label (class_of_level level) (section_conv location order) [pcdata settings.names.bibliography_name]
			| `Section (label, order, location, level, `Notes) ->
				make_toc_entry label (class_of_level level) (section_conv location order) [pcdata settings.names.notes_name]
			| `Section (label, order, location, level, `Toc) ->
				make_toc_entry label (class_of_level level) (section_conv location order) [pcdata settings.names.toc_name]


	in XHTML.M.div ~a:[a_class ["doc"; "doc_valid"; classname]] (write_frag content)


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
			| hd::tl	-> XHTML.M.ul ~a:[a_class ["doc_error_lines"]] hd tl

	in XHTML.M.li ~a:[a_class ["doc_error"]]
		[
		XHTML.M.h1 ~a:[a_class ["doc_error_head"]] [pcdata (sprintf "Error in line %d:" line_number)];
		show_context;
		XHTML.M.p ~a:[a_class ["doc_error_msg"]] [pcdata (Explanations.explain_error error_msg)]
		]


let write_invalid_document classname = function
	| []		-> raise Empty_error_list
	| hd::tl	-> div ~a:[a_class ["doc"; "doc_invalid"; classname]]
				[ul ~a:[a_class ["doc_errors"]] (write_error hd) (List.map write_error tl)]


(********************************************************************************)
(**	{3 Classnames}								*)
(********************************************************************************)

let write_ambivalent_document settings classname = function
	| `Valid doc	-> write_valid_document settings classname doc
	| `Invalid doc	-> write_invalid_document classname doc


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

