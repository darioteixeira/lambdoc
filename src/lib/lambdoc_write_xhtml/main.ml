(********************************************************************************)
(*	Main.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
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
open Prelude
open Basic
open Source
open Valid
open Lambdoc_writer
open Writeconv
open Settings
open Translations


(********************************************************************************)
(**	{1 Exceptions}								*)
(********************************************************************************)

exception Command_see_with_non_note of Target.t
exception Command_cite_with_non_bib of Target.t
exception Command_ref_with_non_visible_block of Target.t
exception Command_sref_with_non_visible_block of Target.t


(********************************************************************************)
(**	{1 Private type definitions}						*)
(********************************************************************************)

type name_t =
	| Name_custom of string
	| Name_equation
	| Name_printout
	| Name_table
	| Name_figure
	| Name_part
	| Name_appendix
	| Name_section
	| Name_bibliography
	| Name_notes
	| Name_toc
	| Name_abstract


(********************************************************************************)
(**	{1 Private functions and values}					*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Document classname prefix}						*)
(********************************************************************************)

let doc_prefix = "doc"


let (!!) str = "doc_" ^ str


let (^^) s1 s2 = "doc_" ^ s1 ^ s2


(********************************************************************************)
(**	{2 Helper functions}							*)
(********************************************************************************)

let make_label = function
	| `Auto_label ref -> "doc:a:" ^ ref
	| `User_label ref -> "doc:u:" ^ ref


let make_link ?classname lnk content =
	let class_attr = a_class (!!"lnk" :: match classname with None -> [] | Some x -> [x]) in
	let attr = class_attr :: [a_href (uri_of_string lnk)]
	in XHTML.M.a ~a:attr content


let make_external_link = make_link


let make_internal_link ?classname ref content = make_link ?classname ("#" ^ (make_label ref)) content


let cons_of_level = function
	| `Level1 -> XHTML.M.h1
	| `Level2 -> XHTML.M.h2
	| `Level3 -> XHTML.M.h3


let class_of_level = function
	| `Level0 -> !!"level0"
	| `Level1 -> !!"level1"
	| `Level2 -> !!"level2"
	| `Level3 -> !!"level3"


let make_floatation floatation = ["float_" ^^ (Basic_output.string_of_floatation floatation)]
	

let make_heading cons label orderlst classname content =
	cons ?a:(Some [a_id (make_label label); a_class [classname]]) (orderlst @ [span content])


let make_sectional level label orderlst content =
	make_heading (cons_of_level level) label orderlst !!"sec" content


(********************************************************************************)
(**	{2 Converters}								*)
(********************************************************************************)

let listify_order ?(spanify = false) ?(prespace = false) ?order_prefix order =
	let content = match (order_prefix, order) with
		| (Some p, Some o) -> Some (p ^ o)
		| (None, Some o)   -> Some o
		| _		   -> None in
	let bundle = match (content, prespace) with
		| (Some c, true)  -> [space (); pcdata c]
		| (Some c, false) -> [pcdata c]
		| (None, _)	  -> []
	in match (bundle, spanify) with
		| ([], _)    -> []
		| (b, true)  -> [span ~a:[a_class [!!"order"]] b]
		| (b, false) -> b


let part_conv ?spanify ?prespace order = listify_order ?spanify ?prespace (Order_output.maybe_string_of_ordinal Order_output.format_roman order)


let section_conv ?spanify ?prespace location order =
	let conv = match location with
		| `Mainbody   -> Order_output.format_mainbody
		| `Appendixed -> Order_output.format_appendixed
	in listify_order ?spanify ?prespace (Order_output.maybe_string_of_hierarchical conv order)


let boxout_conv ?prespace order = listify_order ?prespace ~order_prefix:"#" (Order_output.maybe_string_of_ordinal Order_output.format_arabic order)


let theorem_conv ?prespace order = listify_order ?prespace (Order_output.maybe_string_of_ordinal Order_output.format_arabic order)


let wrapper_conv ?prespace order = listify_order ?prespace (Order_output.maybe_string_of_ordinal Order_output.format_arabic order)


let bib_conv order = listify_order (Order_output.maybe_string_of_ordinal Order_output.format_arabic order)


let note_conv order = listify_order (Order_output.maybe_string_of_ordinal Order_output.format_arabic order)


(********************************************************************************)
(**	{2 Conversion of valid documents}					*)
(********************************************************************************)

let write_valid_document ?(translations = Translations.default) ?(settings = Settings.default) ?(lookup = XHTML.M.uri_of_string) ?(default_classnames = [doc_prefix; !!"valid"]) classname doc =

	(************************************************************************)
	(* Predefined sequences with last question and answer.			*)
	(************************************************************************)

	let last_question_seq = ref (Some (`Plain "Q:", []))
	and last_answer_seq = ref (Some (`Plain "A:", []))


	(************************************************************************)
	(* Fetch fields from doc record.					*)
	(************************************************************************)

	and content = doc.Valid.content
	and bibs = doc.Valid.bibs
	and notes = doc.Valid.notes
	and toc = doc.Valid.toc
	and labels = doc.Valid.labels
	and custom = doc.Valid.custom in


	(************************************************************************)
	(* Converters for inline context.					*)
	(************************************************************************)

	let rec write_seq seq =
		let (hd, tl) = nemap write_inline seq
		in hd :: tl


	and write_inline = function

		| `Plain txt ->
			XHTML.M.pcdata txt

		| `Entity txt ->
			XHTML.M.entity txt

		| `Linebreak ->
			XHTML.M.br ()

		| `Math math ->
			let xhtml : [> `Span ] XHTML.M.elt = XHTML.M.unsafe_data (Math_output.get_mathml math)
			in XHTML.M.span ~a:[a_class [!!"mathinl"]] [xhtml]

		| `Glyph (alias, alt) ->
			XHTML.M.img ~a:[a_class [!!"glyph"]] ~src:(lookup alias) ~alt ()

		| `Bold seq ->
			XHTML.M.b ~a:[a_class [!!"bold"]] (write_seq seq)

		| `Emph seq ->
			XHTML.M.i ~a:[a_class [!!"emph"]] (write_seq seq)

		| `Code seq ->
			XHTML.M.tt ~a:[a_class [!!"code"]] (write_seq seq)

		| `Caps seq ->
			XHTML.M.span ~a:[a_class [!!"caps"]] (write_seq seq)

		| `Ins seq ->
			XHTML.M.ins ~a:[a_class [!!"ins"]] (write_seq seq)

		| `Del seq ->
			XHTML.M.del ~a:[a_class [!!"del"]] (write_seq seq)

		| `Sup seq ->
			XHTML.M.sup ~a:[a_class [!!"sup"]] (write_seq seq)

		| `Sub seq ->
			XHTML.M.sub ~a:[a_class [!!"sub"]] (write_seq seq)

		| `Mbox seq ->
			XHTML.M.span ~a:[a_class [!!"mbox"]] (write_seq seq)

		| `Span (classname, seq) ->
			let a = maybe (fun x -> [a_class ["span_" ^^ x]]) classname
			in XHTML.M.span ?a (write_seq seq)

		| `Link (lnk, None) ->
			make_external_link lnk (Obj.magic (write_seq (`Plain lnk, [])))

		| `Link (lnk, Some seq) ->
			make_external_link lnk (Obj.magic (write_seq seq))

		| `See (hd, tl) ->
			let link_maker ref =
				let label = `User_label ref in
				let target = Hashtbl.find labels label
				in (match target with
					| Target.Note_target order -> make_internal_link label (note_conv order)
					| _			   -> raise (Command_see_with_non_note target)) in
			let commafy ref = [pcdata ","; link_maker ref]
			in XHTML.M.span ~a:[a_class [!!"see"]] ((pcdata "(") :: (link_maker hd) :: (List.flatten (List.map commafy tl)) @ [pcdata ")"])

		| `Cite (hd, tl) ->
			let link_maker ref =
				let label = `User_label ref in
				let target = Hashtbl.find labels label
				in match target with
					| Target.Bib_target order -> make_internal_link label (bib_conv order)
					| _			  -> raise (Command_cite_with_non_bib target) in
			let commafy ref = [pcdata ","; link_maker ref]
			in XHTML.M.span ~a:[a_class [!!"cite"]] ((pcdata "[") :: (link_maker hd) :: (List.flatten (List.map commafy tl)) @ [pcdata "]"])

		| `Ref ref ->
			let label = `User_label ref in
			let target = Hashtbl.find labels label
			in (match target with
				| Target.Visible_target (Target.Custom_target (_, Custom.Boxout, order)) ->
					make_internal_link label (boxout_conv order)
				| Target.Visible_target (Target.Custom_target (_, Custom.Theorem, order)) ->
					make_internal_link label (theorem_conv order)
				| Target.Visible_target (Target.Wrapper_target (_, order)) ->
					make_internal_link label (wrapper_conv order)
				| Target.Visible_target (Target.Part_target order) ->
					make_internal_link label (part_conv order)
				| Target.Visible_target (Target.Section_target (location, order)) ->
					make_internal_link label (section_conv location order)
				| _ ->
					raise (Command_ref_with_non_visible_block target))

		| `Sref ref ->
			let target = Hashtbl.find labels (`User_label ref) in
			let make_sref wseq order =
				make_internal_link (`User_label ref) (Obj.magic (wseq @ order))
			in (match target with
				| Target.Visible_target (Target.Custom_target (env, Custom.Boxout, order)) ->
					make_sref (write_name (Name_custom env)) (boxout_conv ~prespace:true order)
				| Target.Visible_target (Target.Custom_target (env, Custom.Theorem, order)) ->
					make_sref (write_name (Name_custom env)) (theorem_conv ~prespace:true order)
				| Target.Visible_target (Target.Wrapper_target (Wrapper.Equation, order)) ->
					make_sref (write_name Name_equation) (wrapper_conv ~prespace:true order)
				| Target.Visible_target (Target.Wrapper_target (Wrapper.Printout, order)) ->
					make_sref (write_name Name_printout) (wrapper_conv ~prespace:true order)
				| Target.Visible_target (Target.Wrapper_target (Wrapper.Table, order)) ->
					make_sref (write_name Name_table) (wrapper_conv ~prespace:true order)
				| Target.Visible_target (Target.Wrapper_target (Wrapper.Figure, order)) ->
					make_sref (write_name Name_figure) (wrapper_conv ~prespace:true order)
				| Target.Visible_target (Target.Part_target order) ->
					make_sref (write_name Name_part) (part_conv ~prespace:true order)
				| Target.Visible_target (Target.Section_target (location, order)) ->
					let name = match location with
						| `Mainbody   -> Name_section
						| `Appendixed -> Name_appendix
					in make_sref (write_name name) (section_conv ~prespace:true location order)
				| _ ->
					raise (Command_sref_with_non_visible_block target))

		| `Mref (ref, seq) ->
			make_internal_link (`User_label ref) (Obj.magic (write_seq seq))


	(************************************************************************)
	(* Name converter.							*)
	(************************************************************************)

	and write_name =
		let cache = Hashtbl.create (Hashtbl.length custom)
		in fun name ->
			try Hashtbl.find cache name
			with Not_found ->
				let seq = match name with
					| Name_custom env   -> Hashtbl.find custom env
					| Name_equation	    -> translations.equation
					| Name_printout	    -> translations.printout
					| Name_table	    -> translations.table
					| Name_figure	    -> translations.figure
					| Name_part	    -> translations.part
					| Name_appendix	    -> translations.appendix
					| Name_section	    -> translations.section
					| Name_bibliography -> translations.bibliography
					| Name_notes	    -> translations.notes
					| Name_toc	    -> translations.toc
					| Name_abstract     -> translations.abstract in
				let value = write_seq seq in
				Hashtbl.add cache name value;
				value in


	(************************************************************************)
	(* Converters for tabular environment.					*)
	(************************************************************************)

	let write_tabular tab =

		let write_cell ord (maybe_cellspec, maybe_seq) =
			let ((alignment, weight), maybe_colspan, overline, underline) = match maybe_cellspec with
				| Some (spec, span, overline, underline) -> (spec, Some span, overline, underline)
				| None					 -> (Array.get tab.Tabular.tcols (ord+1), None, false, false) in
			let cell_class = ["cell_" ^^ Tabular_output.string_of_alignment alignment]
			and oline_class = if overline then [!!"oline"] else []
			and uline_class = if underline then [!!"uline"] else [] in
			let a_hd = a_class (cell_class @ oline_class @ uline_class)
			and a_tl = match maybe_colspan with Some n -> [a_colspan n] | None -> []
			and out_seq = match maybe_seq with Some seq -> (write_seq seq) | None -> []
			in match weight with
				| Tabular.Normal -> XHTML.M.td ~a:(a_hd :: a_tl) out_seq
				| Tabular.Strong -> XHTML.M.th ~a:(a_hd :: a_tl) out_seq in

		let write_row (hd, tl) =
			XHTML.M.tr (write_cell (-1) hd) (List.mapi write_cell tl) in

		let write_group (hd, tl) =
			let hd = write_row hd in
			let tl = List.map write_row tl
			in (hd, tl) in

		let thead = match tab.Tabular.thead with
			| None		-> None
			| Some grp	-> let (hd, tl) = write_group grp in Some (XHTML.M.thead ~a:[a_class [!!"tgroup"]] hd tl)

		and (tbody_hd, tbody_tl) =
			let write_tbody grp =
				let (hd, tl) = write_group grp
				in XHTML.M.tbody ~a:[a_class [!!"tgroup"]] hd tl in
			let (hd, tl) = tab.Tabular.tbodies
			in (write_tbody hd, List.map write_tbody tl)

		and tfoot = match tab.Tabular.tfoot with
			| None		-> None
			| Some grp	-> let (hd, tl) = write_group grp in Some (XHTML.M.tfoot ~a:[a_class [!!"tgroup"]] hd tl)

		in XHTML.M.div ~a:[a_class [!!"tab"]] [XHTML.M.div ~a:[a_class [!!"tab_aux"]]  [XHTML.M.tablex ?thead ?tfoot tbody_hd tbody_tl]] in


	(************************************************************************)
	(* Converters for document blocks.					*)
	(************************************************************************)

	let rec write_frag frag =
		let (hd, tl) = nemap write_block frag
		in List.flatten (hd :: tl)


	and write_block = function

		| `Paragraph (initial, indent, seq) ->
			let style_initial = if initial then [!!"initial"] else []
			and style_indent = match indent with
				| Some true  -> [!!"indent"]
				| Some false -> [!!"noindent"]
				| None	     -> [] in
			let style = style_initial @ style_indent
			in [XHTML.M.p ~a:[a_class (!!"par" :: style)] (write_seq seq)]

		| `Itemize (bul, frags) ->
			let (hd, tl) = nemap (fun frag -> XHTML.M.li ~a:[a_class [!!"item"]] (write_frag frag)) frags
			and style = ["style_" ^^ (Basic_output.string_of_bullet bul)]
			in [XHTML.M.ul ~a:[a_class (!!"itemize" :: style)] hd tl]

		| `Enumerate (num, frags) ->
			let (hd, tl) = nemap (fun frag -> XHTML.M.li ~a:[a_class [!!"item"]] (write_frag frag)) frags
			and style = ["style_" ^^ (Basic_output.string_of_numbering num)]
			in [XHTML.M.ol ~a:[a_class (!!"enumerate" :: style)] hd tl]

		| `Description frags ->
			let write_frag (seq, frag) = (XHTML.M.dt ~a:[a_class [!!"item"]] (write_seq seq), XHTML.M.dd (write_frag frag)) in
			let (hd, tl) = nemap write_frag frags in
			let (new_hd, new_tl) =
				let (first, second) = hd
				in (first, second :: (List.flatten (List.map (fun (x, y) -> [x; y]) tl)))
			in [XHTML.M.dl ~a:[a_class [!!"description"]] new_hd new_tl]

		| `Qanda pairs ->
			let write_frag ~qora (maybe_maybe_seq, frag) =
				let qora_class = match qora with
					| `Question -> !!"question"
					| `Answer   -> !!"answer" in
				let maybe_seq = match (maybe_maybe_seq, qora) with
					| (Some maybe_seq, `Question)	-> last_question_seq := maybe_seq; maybe_seq
					| (Some maybe_seq, `Answer)	-> last_answer_seq := maybe_seq; maybe_seq
					| (None, `Question)		-> !last_question_seq
					| (None, `Answer)		-> !last_answer_seq in
				let (outseq, empty_class) = match maybe_seq with
					| Some seq -> (write_seq seq, [!!"empty"])
					| None	   -> ([], [])
				in (XHTML.M.dt ~a:[a_class (qora_class :: empty_class)] outseq, XHTML.M.dd ~a:[a_class [qora_class]] (write_frag frag)) in
			let write_pair (q, a) = (write_frag ~qora:`Question q, write_frag ~qora:`Answer a) in
			let (hd, tl) = nemap write_pair pairs in
			let (new_hd, new_tl) =
				let ((first, second), (third, fourth)) = hd
				in (first, second :: third :: fourth :: (List.flatten (List.map (fun ((q1, q2), (a1, a2)) -> [q1; q2; a1; a2]) tl)))
			in [XHTML.M.dl ~a:[a_class [!!"qanda"]] new_hd new_tl]

		| `Verse frag ->
			[XHTML.M.div ~a:[a_class [!!"verse"]] [XHTML.M.div ~a:[a_class [!!"verse_aux"]] (write_frag frag)]]

		| `Quote frag ->
			[XHTML.M.blockquote ~a:[a_class [!!"quote"]] (write_frag frag)]

		| `Math math ->
			let xhtml : [> `Div ] XHTML.M.elt = XHTML.M.unsafe_data (Math_output.get_mathml math)
			in [XHTML.M.div ~a:[a_class [!!"mathblk"]] [xhtml]]

		| `Source src ->
			let dummy_lines = src.style <> Source.Plain
			and extra_classes = [!!("src_" ^ (Writeconv.Source_output.string_of_style src.style))]
			in [Camlhighlight_write_xhtml.write ~class_prefix:!!"src_" ~extra_classes ~dummy_lines ~linenums:src.linenums src.hilite]

		| `Tabular tab ->
			[write_tabular tab]

		| `Verbatim (mult, txt) ->
			[XHTML.M.div ~a:[a_class [!!"verb"]] [XHTML.M.div ~a:[a_class [!!"verb_aux"]] [XHTML.M.pre ~a:[a_class ["mult" ^^ (string_of_int mult)]] [XHTML.M.pcdata txt]]]]

		| `Picture (frame, width, alias, alt) ->
			let style = if frame then [!!"pic_frame"] else [] in
			let attrs = match width with
				| Some w -> [a_width (`Percent w)]
				| None	 -> [] in
			let uri = lookup alias in
			let img = XHTML.M.a ~a:[a_href uri; a_class [!!"pic_lnk"]] [XHTML.M.img ~a:attrs ~src:uri ~alt ()]
			in [XHTML.M.div ~a:[a_class (!!"pic" :: style)] [img]]

		| `Subpage frag ->
			[XHTML.M.div ~a:[a_class [!!"subpage"]] (write_frag frag)]

		| `Decor (floatation, blk) ->
			let style = make_floatation floatation
			in [XHTML.M.div ~a:[a_class (!!"decor" :: style)] (write_block blk)]

		| `Pullquote (floatation, maybe_seq, frag) ->
			let style = make_floatation floatation
			and head = match maybe_seq with
				| Some seq -> [XHTML.M.h1 ~a:[a_class [!!"pullhead"]] ([entity "mdash"; entity "ensp"] @ (write_seq seq))]
				| None	   -> []
			in [XHTML.M.div ~a:[a_class (!!"pull" :: style)] [XHTML.M.div ~a:[a_class [!!"pull_aux"]] ((write_frag frag) @ head)]]

		| `Boxout (floatation, data, maybe_seq, frag) ->
			let formatter = function
				| Some seq1, Some order, Some seq2 ->
					seq1 @ (boxout_conv ~prespace:true order) @ [pcdata ":"; space ()] @ seq2
				| Some seq1, None, Some seq2 ->
					seq1 @ [pcdata ":"; space ()] @ seq2
				| Some seq1, Some order, None ->
					seq1 @ (boxout_conv ~prespace:true order)
				| Some seq1, None, None ->
					seq1
				| None, None, Some seq2 ->
					seq2
				| _ ->
					[]
			in [write_custom (Some floatation) data maybe_seq frag !!"boxout" formatter]

		| `Theorem (data, maybe_seq, frag) ->
			let formatter triple =
				let (hd, bd) = match triple with
					| Some seq1, Some order, Some seq2 ->
						(seq1 @ (theorem_conv ~prespace:true order), [pcdata "("] @ seq2 @ [pcdata ")."])
					| Some seq1, None, Some seq2 ->
						(seq1,  [pcdata "("] @ seq2 @ [pcdata ")."])
					| Some seq1, Some order, None ->
						(seq1 @ (theorem_conv ~prespace:true order) @ [pcdata "."], [])
					| Some seq1, None, None ->
						(seq1 @ [pcdata "."], [])
					| _ ->
						([], []) in
				let caphead = match hd with
					| [] -> []
					| x  -> [span ~a:[a_class [!!"thmname"]] x]
				and capbody = match bd with
					| [] -> []
					| x  -> [span ~a:[a_class [!!"thmextra"]] x]
				in caphead @ capbody
			in [write_custom None (data :> Custom.t) maybe_seq frag !!"theorem" formatter]

		| `Equation (floatation, wrapper, blk) ->
			[write_wrapper floatation wrapper blk !!"equation" Name_equation]

		| `Printout (floatation, wrapper, blk) ->
			[write_wrapper floatation wrapper blk !!"printout" Name_printout]

		| `Table (floatation, wrapper, blk) ->
			[write_wrapper floatation wrapper blk !!"table" Name_table]

		| `Figure (floatation, wrapper, blk) ->
			[write_wrapper floatation wrapper blk !!"figure" Name_figure]

		| `Heading heading ->
			write_heading_block heading

		| `Title (level, seq) ->
			[(cons_of_level level) ~a:[a_class [!!"title"]] (write_seq seq)]

		| `Abstract frag ->
			[XHTML.M.div ~a:[a_class [!!"abstract"]] ((XHTML.M.h1 ~a:[a_class [!!"sec"]] (write_name Name_abstract)) :: (write_frag frag))]

		| `Rule ->
			[XHTML.M.hr ~a:[a_class [!!"rule"]] ()]


	and write_heading_block = function

		| `Part (label, order, `Custom seq) ->
			[make_heading XHTML.M.h1 label (part_conv ~spanify:true order) !!"part" (write_seq seq)]

		| `Part (label, order, `Appendix) ->
			[make_heading XHTML.M.h1 label (part_conv ~spanify:true order) !!"part" (write_name Name_appendix)]

		| `Section (label, order, location, level, `Custom seq) ->
			[make_sectional level label (section_conv ~spanify:true location order) (write_seq seq)]

		| `Section (label, order, location, level, `Bibliography) ->
			let title = make_sectional level label (section_conv ~spanify:true location order) (write_name Name_bibliography) in
			let bibs = match bibs with
				| []	   -> []
				| hd :: tl -> let (hd, tl) = nemap write_bib (hd, tl) in [XHTML.M.ol ~a:[a_class [!!"bibs"]] hd tl]
			in title::bibs

		| `Section (label, order, location, level, `Notes) ->
			let title = make_sectional level label (section_conv ~spanify:true location order) (write_name Name_notes) in
			let notes = match notes with
				| []	   -> []
				| hd :: tl -> let (hd, tl) = nemap write_note (hd, tl) in [XHTML.M.ol ~a:[a_class [!!"notes"]] hd tl]
			in title::notes

		| `Section (label, order, location, level, `Toc) ->
			let title = make_sectional level label (section_conv ~spanify:true location order) (write_name Name_toc) in
			let entries = List.filter_map write_toc_entry toc in
			let toc_xhtml = match entries with
				| []	   -> []
				| hd :: tl -> [XHTML.M.ul ~a:[a_class [!!"toc"]] hd tl]
			in title::toc_xhtml


	and write_custom maybe_floatation data maybe_seq frag classname formatter =
		let style = match maybe_floatation with
			| Some floatation -> make_floatation floatation
			| None		  -> [] in
		let (env, label, triple) = match data with
			| `Anonymous (env, label)	-> (env, label, (None, None, maybe write_seq maybe_seq))
			| `Unnumbered (env, label)	-> (env, label, (Some (write_name (Name_custom env)), None, maybe write_seq maybe_seq))
			| `Numbered (env, label, order) -> (env, label, (Some (write_name (Name_custom env)), Some order, maybe write_seq maybe_seq)) in
		let title = match formatter triple with
			| [] -> []
			| xs -> [XHTML.M.h1 ~a:[a_class [classname ^ "_head"]] xs] in
		let content = title @ [XHTML.M.div ~a:[a_class [classname ^ "_body"]] (write_frag frag)]
		in XHTML.M.div ~a:[a_id (make_label label); a_class (classname :: (classname ^ "_env_"  ^ env) :: style)] content


	and write_wrapper floatation wrapper blk classname name =
		let wrapper_content = match write_block blk with
			| [b] -> b
			| _   -> failwith "write_wrapper" in
		let (length, label, caption_content) = match wrapper with
			| Wrapper.Ordered (label, order, maybe_seq) ->
				let headcore = (write_name name) @ (wrapper_conv ~prespace:true order)
				in (match maybe_seq with
					| Some seq -> ("long", label, [XHTML.M.h1 ~a:[a_class [!!"caption_head"]] (headcore @ [pcdata ":"]); XHTML.M.p ~a:[a_class [!!"caption_body"]] (write_seq seq)])
					| None	   -> ("short", label, [XHTML.M.h1 ~a:[a_class [!!"caption_head"]] ([pcdata "("] @ headcore @ [pcdata ")"])]))
			| Wrapper.Unordered (label, seq) ->
				("long", label, [XHTML.M.p ~a:[a_class [!!"caption_body"]] (write_seq seq)]) in
		let caption = XHTML.M.div ~a:[a_class [!!"caption"; "caption_" ^^ length]] [XHTML.M.div ~a:[a_class [!!"caption_aux"]] caption_content] in
		let classnames = [!!"wrapper"; classname] @ (make_floatation floatation)
		in XHTML.M.div ~a:[a_id (make_label label); a_class classnames] [wrapper_content; caption]


	(************************************************************************)
	(* Writers for ghost elements: notes, bib entries, and TOC entries.	*)
	(************************************************************************)

	and write_note note =
		XHTML.M.li ~a:[a_id (make_label note.Note.label); a_class [!!"note"]]
			[
			XHTML.M.span ~a:[a_class [!!"note_head"]] (pcdata "(" :: (note_conv note.Note.order) @ [pcdata ")"]);
			XHTML.M.div ~a:[a_class [!!"note_body"]] (write_frag note.Note.content);
			]


	and write_bib bib =
		XHTML.M.li ~a:[a_id (make_label bib.Bib.label); a_class [!!"bib"]]
			[
			XHTML.M.span ~a:[a_class [!!"bib_head"]] (pcdata "[" :: (bib_conv bib.Bib.order) @ [pcdata "]"]);
			XHTML.M.p ~a:[a_class [!!"bib_body"]]
				[
				XHTML.M.span ~a:[a_class [!!"bib_author"]] (write_seq bib.Bib.author);
				XHTML.M.span ~a:[a_class [!!"bib_title"]] (write_seq bib.Bib.title);
				XHTML.M.span ~a:[a_class [!!"bib_resource"]] (write_seq bib.Bib.resource);
				]
			]


	and write_toc_entry sec =
		let make_toc_entry label classname orderlst content =
		        Some (XHTML.M.li ~a:[a_class [!!"item"; classname]] [make_internal_link label (orderlst @ (Obj.magic content))])
		in match sec with
			| `Part (label, order, `Custom seq) ->
				make_toc_entry label (class_of_level `Level0) (part_conv ~spanify:true order) (write_seq seq)
			| `Part (label, order, `Appendix) ->
				make_toc_entry label (class_of_level `Level0) (part_conv ~spanify:true order) (write_name Name_appendix)
			| `Section (label, order, location, level, `Custom seq) ->
				make_toc_entry label (class_of_level level) (section_conv ~spanify:true location order) (write_seq seq)
			| `Section (label, order, location, level, `Bibliography) ->
				make_toc_entry label (class_of_level level) (section_conv ~spanify:true location order) (write_name Name_bibliography)
			| `Section (label, order, location, level, `Notes) ->
				make_toc_entry label (class_of_level level) (section_conv ~spanify:true location order) (write_name Name_notes)
			| `Section (label, order, location, level, `Toc) ->
				make_toc_entry label (class_of_level level) (section_conv ~spanify:true location order) (write_name Name_toc)


	in XHTML.M.div ~a:[a_class (classname :: default_classnames)] (write_frag content)


(********************************************************************************)
(**	{2 Conversion of invalid documents}					*)
(********************************************************************************)

let write_error (maybe_error_context, error_msg) =
	let context = match maybe_error_context with
		| Some error_context ->
			let line_number = error_context.Error.error_line_number
			and line_before = error_context.Error.error_line_before
			and line_actual = error_context.Error.error_line_actual
			and line_after = error_context.Error.error_line_after in
			let show_line classname delta line =
				XHTML.M.li ~a:[a_class [classname]]
					[
					span ~a:[a_class [!!"error_linenum"]] [pcdata (sprintf "%03d" (line_number + delta))];
					span ~a:[a_class [!!"error_linestr"]] [pcdata line]
					] in
			let show_line_around delta line =
				show_line !!"error_around" delta line
			and show_line_actual =
				show_line !!"error_actual" 0 line_actual in
			let lines =
				(List.mapi (fun ord line -> show_line_around (ord - (List.length line_before)) line) line_before) @
				[show_line_actual] @
				(List.mapi (fun ord line -> show_line_around (ord+1) line) line_after)
			in	[
				XHTML.M.h1 ~a:[a_class [!!"error_head"]] [pcdata (sprintf "Error in line %d:" line_number)];
				XHTML.M.ul ~a:[a_class [!!"error_lines"]] (List.hd lines) (List.tl lines);
				]
		| None ->
			[XHTML.M.h1 ~a:[a_class [!!"error_head"]] [pcdata "Global error:"]]
	and explanation_doc = Valid.make_composition (Block.paragraph false None (Explanations.explain error_msg), []) [] in
	let explanation_out = [write_valid_document ~default_classnames:[] !!"error_msg" explanation_doc]
	in XHTML.M.li ~a:[a_class [!!"error"]] (context @ explanation_out)


let write_invalid_document ?(default_classnames = [doc_prefix; !!"invalid"]) classname errors =
	let (hd, tl) = nemap write_error errors
	in div ~a:[a_class (classname :: default_classnames)] [ul ~a:[a_class [!!"errors"]] hd tl]


(********************************************************************************)
(**	{2 High-level interface}						*)
(********************************************************************************)

let write_ambivalent_document ?translations ?settings ?lookup classname = function
	| `Valid doc	-> write_valid_document ?translations ?settings ?lookup classname doc
	| `Invalid doc	-> write_invalid_document classname doc


let manuscript_classname = !!"manuscript"


let composition_classname = !!"composition"


(********************************************************************************)
(**	{1 Public types and functions}						*)
(********************************************************************************)

(**	The following types and functions conform to the Document_writer.S
	signature.
*)

type t = [ `Div ] XHTML.M.elt

let write_ambivalent_manuscript ?translations ?settings ?lookup doc =
	write_ambivalent_document ?translations ?settings ?lookup manuscript_classname doc

let write_ambivalent_composition ?translations ?settings ?lookup doc =
	write_ambivalent_document ?translations ?settings ?lookup composition_classname doc

let write_valid_manuscript ?translations ?settings ?lookup doc =
	write_valid_document ?translations ?settings ?lookup manuscript_classname doc

let write_valid_composition ?translations ?settings ?lookup doc =
	write_valid_document ?translations ?settings ?lookup composition_classname doc

let write_invalid_manuscript doc =
	write_invalid_document manuscript_classname doc

let write_invalid_composition doc =
	write_invalid_document composition_classname doc

