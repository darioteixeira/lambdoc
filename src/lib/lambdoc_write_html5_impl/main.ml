(********************************************************************************)
(*	Main.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Printf
open Lambdoc_core
open Basic
open Inline
open Block
open Heading
open Source
open Lambdoc_writer
open Writeconv

module List = BatList
module String = BatString


(********************************************************************************)
(**	{1 Public functors}							*)
(********************************************************************************)

module Make (Html5: Html5_sigs.T with type 'a Xml.wrap = 'a and type 'a wrap = 'a and type 'a list_wrap = 'a list) =
struct

open Html5


(********************************************************************************)
(**	{2 Private modules}							*)
(********************************************************************************)

module Source_writer = Camlhighlight_write_html5.Make (Html5)


(********************************************************************************)
(**	{2 Exceptions}								*)
(********************************************************************************)

exception Command_see_with_non_note of Target.t
exception Command_cite_with_non_bib of Target.t
exception Command_ref_with_non_visible_block of Target.t
exception Command_sref_with_non_visible_block of Target.t


(********************************************************************************)
(**	{2 Private type definitions}						*)
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
(**	{2 Public type definitions}						*)
(********************************************************************************)

type t = Html5_types.div Html5.elt

type valid_options_t =
	{
	numbered_paragraphs: bool;
	translations: Translations.t;
	namespace: Html5_types.nmtoken;
	prefix: Html5_types.nmtoken;
	base_classes: Html5_types.nmtokens;
	extra_classes: Html5_types.nmtokens;
	}

type invalid_options_t =
	{
	prefix: Html5_types.nmtoken;
	base_classes: Html5_types.nmtokens;
	extra_classes: Html5_types.nmtokens;
	}


(********************************************************************************)
(**	{2 Private functions and values}					*)
(********************************************************************************)

let maybe f = function
	| Some x -> Some (f x)
	| None   -> None


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

let default_valid_options =
	{
	numbered_paragraphs = false;
	translations = Translations.default;
	namespace = "doc";
	prefix = "doc";
	base_classes = ["valid"];
	extra_classes = [];
	}

let default_invalid_options =
	{
	prefix = "doc";
	base_classes = ["invalid"];
	extra_classes = [];
	}


(********************************************************************************)
(**	{3 Conversion of valid documents}					*)
(********************************************************************************)

let from_valid ?(valid_options = default_valid_options) ~link_dict ~image_dict doc =

	let opts = valid_options in


	(************************************************************************)
	(*	{4 Fetch fields from doc record}				*)
	(************************************************************************)

	let content = Valid.(doc.content) in
	let bibs = Valid.(doc.bibs) in
	let notes = Valid.(doc.notes) in
	let toc = Valid.(doc.toc) in
	let labels = Valid.(doc.labels) in
	let customs = Valid.(doc.customs) in


	(************************************************************************)
	(**	{4 Document classname prefix}					*)
	(************************************************************************)

	let (!!) =
		let prefix = opts.prefix ^ "_" in
		fun str -> prefix ^ str in

	let (!!!) =
		let prefix = opts.prefix ^ "_class_" in
		fun str -> prefix ^ str in

	let (^^) =
		let prefix = opts.prefix ^ "_" in
		fun s1 s2 -> prefix ^ s1 ^ s2 in


	(************************************************************************)
	(**	{4 Helper functions}						*)
	(************************************************************************)

	let make_label = function
		| Label.Auto pointer -> opts.namespace ^ ":a:" ^ pointer
		| Label.User pointer -> opts.namespace ^ ":u:" ^ pointer in

	let make_link ?(attr = []) lnk content =
		Html5.a ~a:[a_class (!!"lnk" :: attr); a_href (Html5.uri_of_string lnk)] content in

	let make_internal_link ?attr pointer content =
		make_link ?attr ("#" ^ (make_label pointer)) content in

	let cons_of_level = function
		| 1 -> Html5.h1
		| 2 -> Html5.h2
		| 3 -> Html5.h3
		| 4 -> Html5.h4
		| 5 -> Html5.h5
		| 6 -> Html5.h6
		| _ -> assert false in

	let class_of_level level = !!("level" ^ string_of_int level) in

	let make_heading cons label orderlst classname attr content =
		cons ?a:(Some [a_id (make_label label); a_class (classname :: attr)]) (orderlst @ [Html5.span content]) in

	let make_sectional level label orderlst attr content =
		make_heading (cons_of_level (level : Level.section_t :> int)) label orderlst !!"sec" attr content in

	let make_floatable forbidden =
		if forbidden then [] else [!!"floatable"] in

	let commafy ~prefix ~suffix xs =
		let rec aux accum = function
			| []	   -> assert false
			| [x]	   -> pcdata prefix :: List.rev (pcdata suffix :: x :: accum)
			| hd :: tl -> aux (pcdata "," :: hd :: accum) tl in
		aux [] xs in


	(************************************************************************)
	(**	{4 Converters}							*)
	(************************************************************************)

	let listify_order ?(spanify = false) ?(prespace = false) ?order_prefix order =
		let content = match (order_prefix, order) with
			| (Some p, Some o) -> Some (p ^ o)
			| (None, Some o)   -> Some o
			| _		   -> None in
		let bundle = match (content, prespace) with
			| (Some c, true)  -> [Html5.entity "#xa0"; pcdata c]
			| (Some c, false) -> [pcdata c]
			| (None, _)	  -> []
		in match (bundle, spanify) with
			| ([], _)    -> []
			| (b, true)  -> [Html5.span ~a:[a_class [!!"order"]] b]
			| (b, false) -> b in

	let part_conv ?spanify ?prespace order =
		listify_order ?spanify ?prespace (Order_output.maybe_string_of_ordinal Order_output.format_roman order) in

	let section_conv ?spanify ?prespace location order =
		let conv = match location with
			| Mainbody   -> Order_output.format_mainbody
			| Appendixed -> Order_output.format_appendixed
		in listify_order ?spanify ?prespace (Order_output.maybe_string_of_hierarchical conv order) in

	let boxout_conv ?prespace order =
		listify_order ?prespace ~order_prefix:"#" (Order_output.maybe_string_of_ordinal Order_output.format_arabic order) in

	let theorem_conv ?prespace order =
		listify_order ?prespace (Order_output.maybe_string_of_ordinal Order_output.format_arabic order) in

	let wrapper_conv ?prespace order =
		listify_order ?prespace (Order_output.maybe_string_of_ordinal Order_output.format_arabic order) in

	let bib_conv order =
		listify_order (Order_output.maybe_string_of_ordinal Order_output.format_arabic order) in

	let note_conv order =
		listify_order (Order_output.maybe_string_of_ordinal Order_output.format_arabic order) in


	(************************************************************************)
	(*	{4 Counters}							*)
	(************************************************************************)

	let paragraph_counter = ref 0 in


	(************************************************************************)
	(* 	{4 Predefined sequences for Q&A}				*)
	(************************************************************************)

	let last_question_seq = ref None in
	let last_answer_seq = ref None in


	(************************************************************************)
	(*	{4 Writers for inline context}					*)
	(************************************************************************)

	let rec write_seq seq =
		List.map write_inline seq

	and write_inline {inl; attr} =
		let attr = List.map (!!!) attr in
		match inl with

		| Plain txt ->
			Html5.pcdata txt

		| Entity txt ->
			Html5.entity txt

		| Linebreak ->
			Html5.br ~a:[a_class attr] ()

		| Mathinl math ->
			let html: [> Html5_types.span ] Html5.elt = Html5.Unsafe.data (Math_output.get_mathml math) in
			Html5.span ~a:[a_class (!!"mathinl" :: attr)] [html]

		| Glyph (href, alt) ->
			let href = try Hashtbl.find image_dict href with Not_found -> href in
			let uri = Html5.uri_of_string href in
			Html5.img ~a:[a_class (!!"glyph" :: attr)] ~src:uri ~alt ()

		| Bold seq ->
			Html5.b ~a:[a_class (!!"bold" :: attr)] (write_seq seq)

		| Emph seq ->
			Html5.i ~a:[a_class (!!"emph" :: attr)] (write_seq seq)

		| Code seq ->
			Html5.code ~a:[a_class (!!"code" :: attr)] (write_seq seq)

		| Caps seq ->
			Html5.span ~a:[a_class (!!"caps" :: attr)] (write_seq seq)

		| Ins seq ->
			Html5.ins ~a:[a_class (!!"ins" :: attr)] (write_seq seq)

		| Del seq ->
			Html5.del ~a:[a_class (!!"del" :: attr)] (write_seq seq)

		| Sup seq ->
			Html5.sup ~a:[a_class (!!"sup" :: attr)] (write_seq seq)

		| Sub seq ->
			Html5.sub ~a:[a_class (!!"sub" :: attr)] (write_seq seq)

		| Mbox seq ->
			Html5.span ~a:[a_class (!!"mbox" :: attr)] (write_seq seq)

		| Span seq ->
			Html5.span ~a:[a_class attr] (write_seq seq)

		| Link (href, maybe_seq) ->
			let href' = try Hashtbl.find link_dict href with Not_found -> href in
			let seq = match maybe_seq with
				| Some seq -> seq
				| None -> [Inline.plain href] in
			make_link ~attr href' (Obj.magic (write_seq seq))

		| See pointers ->
			let link_maker pointer =
				let label = Label.User pointer in
				let target = Hashtbl.find labels label in
				match target with
					| Target.Note_target order -> make_internal_link label (note_conv order)
					| _			   -> raise (Command_see_with_non_note target) in
			Html5.span ~a:[a_class (!!"see" :: attr)] (commafy ~prefix:"(" ~suffix:")" (List.map link_maker pointers))

		| Cite pointers ->
			let link_maker pointer =
				let label = Label.User pointer in
				let target = Hashtbl.find labels label in
				match target with
					| Target.Bib_target order -> make_internal_link label (bib_conv order)
					| _			  -> raise (Command_cite_with_non_bib target) in
			Html5.span ~a:[a_class (!!"cite" :: attr)] (commafy ~prefix:"[" ~suffix:"]" (List.map link_maker pointers))

		| Dref (pointer, maybe_seq) ->
			let label = Label.User pointer in
			let target = Hashtbl.find labels label in
			let suffix = match maybe_seq with Some seq -> write_seq seq | None -> [] in
			let make_dref order = make_internal_link ~attr label (Obj.magic (order @ suffix)) in
			begin match target with
				| Target.Visible_target (Target.Custom_target (_, Custom.Boxout, order)) ->
					make_dref (boxout_conv order)
				| Target.Visible_target (Target.Custom_target (_, Custom.Theorem, order)) ->
					make_dref (theorem_conv order)
				| Target.Visible_target (Target.Wrapper_target (_, order)) ->
					make_dref (wrapper_conv order)
				| Target.Visible_target (Target.Part_target order) ->
					make_dref (part_conv order)
				| Target.Visible_target (Target.Section_target (location, order)) ->
					make_dref (section_conv location order)
				| _ ->
					raise (Command_ref_with_non_visible_block target)
			end

		| Sref (pointer, maybe_seq) ->
			let label = Label.User pointer in
			let target = Hashtbl.find labels label in
			let suffix = match maybe_seq with Some seq -> write_seq seq | None -> [] in
			let make_sref wseq order = make_internal_link ~attr label (Obj.magic (wseq @ order @ suffix)) in
			begin match target with
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
						| Mainbody   -> Name_section
						| Appendixed -> Name_appendix in
					make_sref (write_name name) (section_conv ~prespace:true location order)
				| _ ->
					raise (Command_sref_with_non_visible_block target)
			end

		| Mref (pointer, seq) ->
			make_internal_link (Label.User pointer) (Obj.magic (write_seq seq))


	(************************************************************************)
	(*	{4 Name writer}							*)
	(************************************************************************)

	and write_name =
		let cache = Hashtbl.create (Hashtbl.length customs) in
		fun name ->
			try Hashtbl.find cache name
			with Not_found ->
				let open Translations in
				let seq = match name with
					| Name_custom env   -> Hashtbl.find customs env
					| Name_equation	    -> opts.translations.equation
					| Name_printout	    -> opts.translations.printout
					| Name_table	    -> opts.translations.table
					| Name_figure	    -> opts.translations.figure
					| Name_part	    -> opts.translations.part
					| Name_appendix	    -> opts.translations.appendix
					| Name_section	    -> opts.translations.section
					| Name_bibliography -> opts.translations.bibliography
					| Name_notes	    -> opts.translations.notes
					| Name_toc	    -> opts.translations.toc
					| Name_abstract     -> opts.translations.abstract in
				let value = write_seq seq in
				Hashtbl.add cache name value;
				value in


	(************************************************************************)
	(*	{4 Writers for tabular environment}				*)
	(************************************************************************)

	let write_tabular tab =

		let write_cell ord (maybe_cellspec, maybe_seq) =
			let ((alignment, weight), maybe_colspan, overline, underline) = match maybe_cellspec with
				| Some (spec, span, overline, underline) -> (spec, Some span, overline, underline)
				| None					 -> (Array.get tab.Tabular.tcols ord, None, false, false) in
			let cell_class = ["cell_" ^^ Tabular_output.string_of_alignment alignment] in
			let oline_class = if overline then [!!"oline"] else [] in
			let uline_class = if underline then [!!"uline"] else [] in
			let a_hd = a_class (cell_class @ oline_class @ uline_class) in
			let a_tl = match maybe_colspan with Some n -> [a_colspan n] | None -> [] in
			let out_seq = match maybe_seq with Some seq -> write_seq seq | None -> [] in
			match weight with
				| Tabular.Normal -> Html5.td ~a:(a_hd :: a_tl) (out_seq :> Html5_types.td_content_fun Html5.elt list)
				| Tabular.Strong -> Html5.th ~a:(a_hd :: a_tl) (out_seq :> Html5_types.th_content_fun Html5.elt list) in

		let write_row cells =
			Html5.tr (List.mapi write_cell cells) in

		let write_group grp =
			List.map write_row grp in

		let thead = match tab.Tabular.thead with
			| None	   -> None
			| Some grp -> Some (Html5.thead ~a:[a_class [!!"tgroup"]] (write_group grp)) in

		let tbodies =
			let write_tbody grp =
				Html5.tbody ~a:[a_class [!!"tgroup"]] (write_group grp) in
			List.map write_tbody tab.Tabular.tbodies in

		let tfoot = match tab.Tabular.tfoot with
			| None	   -> None
			| Some grp -> Some (Html5.tfoot ~a:[a_class [!!"tgroup"]] (write_group grp)) in

		Html5.div ~a:[a_class [!!"tab"]] [Html5.div ~a:[a_class [!!"tab_aux"]]  [Html5.tablex ?thead ?tfoot tbodies]] in


	(************************************************************************)
	(*	{4 Writers for document blocks}					*)
	(************************************************************************)

	let rec write_frag frag =
		List.flatten (List.map (write_block ~wrapped:false) frag)


	and write_block ?(wrapped = false) {blk; attr} =
		let attr = List.map (!!!) attr in
		match blk with

		| Paragraph seq ->
			let extra =
				if opts.numbered_paragraphs
				then (incr paragraph_counter; [a_title (Translations.(opts.translations.paragraph) ^ " #" ^ (string_of_int !paragraph_counter))])
				else [] in
			[Html5.p ~a:(a_class (!!"par" :: attr) :: extra) (write_seq seq)]

		| Itemize frags ->
			let xs = List.map (fun frag -> Html5.li ~a:[a_class [!!"item"]] (write_frag frag)) frags in
			[Html5.ul ~a:[a_class (!!"itemize" :: attr)] xs]

		| Enumerate frags ->
			let xs = List.map (fun frag -> Html5.li ~a:[a_class [!!"item"]] (write_frag frag)) frags in
			[Html5.ol ~a:[a_class (!!"enumerate" :: attr)] xs]

		| Description elems ->
			let write_dfrag (seq, frag) accum =
				let dt = Html5.dt ~a:[a_class [!!"item"]] (write_seq seq) in
				let dd = Html5.dd ~a:[a_class [!!"item"]] (write_frag frag) in
				dt :: dd :: accum in
			[Html5.dl ~a:[a_class (!!"description" :: attr)] (List.fold_right write_dfrag elems [])]

		| Qanda elems ->
			let write_qfrag (qanda, frag) accum =
				let (qora, maybe_seq) = match qanda with
					| Qanda.New_questioner maybe_seq -> last_question_seq := maybe_seq; (`Question, maybe_seq)
					| Qanda.New_answerer maybe_seq	 -> last_answer_seq := maybe_seq; (`Answer, maybe_seq)
					| Qanda.Same_questioner		 -> (`Question, !last_question_seq)
					| Qanda.Same_answerer		 -> (`Answer, !last_answer_seq) in
				let qora_class = match qora with
					| `Question -> !!"question"
					| `Answer   -> !!"answer" in
				let (outseq, empty_class) = match maybe_seq with
					| Some seq -> (write_seq seq, [!!"empty"])
					| None	   -> ([], []) in
				let dt = Html5.dt ~a:[a_class (qora_class :: empty_class)] outseq in
				let dd = Html5.dd ~a:[a_class [qora_class]] (write_frag frag) in
				dt :: dd :: accum in
			[Html5.dl ~a:[a_class (!!"qanda" :: attr)] (List.fold_right write_qfrag elems [])]

		| Verse frag ->
			let aux = Html5.div ~a:[a_class [!!"verse_aux"]] (write_frag frag) in
			[Html5.div ~a:[a_class (!!"verse" :: attr)] [aux]]

		| Quote frag ->
			[Html5.blockquote ~a:[a_class (!!"quote" :: attr)] (write_frag frag)]

		| Mathblk math ->
			let html: [> Html5_types.div ] Html5.elt = Html5.Unsafe.data (Math_output.get_mathml math) in
			[Html5.div ~a:[a_class (!!"mathblk" :: attr)] [html]]

		| Source src ->
			[Source_writer.write ~class_prefix:!!"src_" ~extra_classes:attr ~linenums:src.linenums src.hilite]

		| Tabular tab ->
			[write_tabular tab]

		| Subpage frag ->
			[Html5.div ~a:[a_class (!!"subpage" :: attr)] (write_frag frag)]

		| Verbatim txt ->
			let aux = Html5.div ~a:[a_class [!!"pre_aux"]] [Html5.pre ~a:[a_class [!!"pre_aux"]] [Html5.pcdata txt]] in
			[Html5.div ~a:[a_class (!!"pre" :: attr @ make_floatable wrapped)] [aux]]

		| Picture (href, alt, width) ->
			let wattr = match width with Some w -> [a_width w] | None -> [] in
			let href = try Hashtbl.find image_dict href with Not_found -> href in
			let uri = Html5.uri_of_string href in
			let img = Html5.a ~a:[a_href uri; a_class [!!"pic_lnk"]] [Html5.img ~a:(a_class [!!"pic"] :: wattr) ~src:uri ~alt ()] in
			[Html5.div ~a:[a_class (!!"pic" :: attr @ make_floatable wrapped)] [img]]

		| Pullquote (maybe_seq, frag) ->
			let head = match maybe_seq with
				| Some seq -> [Html5.h1 ~a:[a_class [!!"pull_head"]] ([Html5.entity "#x2014"; Html5.entity "#x2002"] @ (write_seq seq))]
				| None	   -> [] in
			let aux = Html5.div ~a:[a_class [!!"pull_aux"]] ((write_frag frag) @ head) in
			[Html5.div ~a:[a_class (!!"pull" :: attr @ make_floatable false)] [aux]]

		| Boxout (data, maybe_seq, frag) ->
			let formatter = function
				| Some seq1, Some order, Some seq2 ->
					seq1 @ (boxout_conv ~prespace:true order) @ [pcdata ":"; Html5.entity "#xa0"] @ seq2
				| Some seq1, None, Some seq2 ->
					seq1 @ [pcdata ":"; Html5.entity "#xa0"] @ seq2
				| Some seq1, Some order, None ->
					seq1 @ (boxout_conv ~prespace:true order)
				| Some seq1, None, None ->
					seq1
				| None, None, Some seq2 ->
					seq2
				| _ ->
					[] in
			[write_custom data maybe_seq frag !!"boxout" (attr @ make_floatable false) formatter]

		| Theorem (data, maybe_seq, frag) ->
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
					| x  -> [Html5.span ~a:[a_class [!!"thmname"]] x]
				and capbody = match bd with
					| [] -> []
					| x  -> [Html5.span ~a:[a_class [!!"thmextra"]] x]
				in caphead @ capbody in
			[write_custom (data :> Custom.t) maybe_seq frag !!"theorem" attr formatter]

		| Equation (wrapper, blk) ->
			[write_wrapper wrapper blk !!"equation" attr Name_equation]

		| Printout (wrapper, blk) ->
			[write_wrapper wrapper blk !!"printout" attr Name_printout]

		| Table (wrapper, blk) ->
			[write_wrapper wrapper blk !!"table" attr Name_table]

		| Figure (wrapper, blk) ->
			[write_wrapper wrapper blk !!"figure" attr Name_figure]

		| Heading heading ->
			write_heading_block attr heading

		| Title (level, seq) ->
			[(cons_of_level (level :> int)) ~a:[a_class (!!"title" :: attr)] (write_seq seq)]

		| Abstract frag ->
			let aux = Html5.h1 ~a:[a_class [!!"sec"]] (write_name Name_abstract) :: (write_frag frag) in
			[Html5.div ~a:[a_class (!!"abstract" :: attr)] aux]

		| Rule ->
			[Html5.hr ~a:[a_class (!!"rule" :: attr)] ()]


	and write_heading_block attr = function

		| Part (label, order, Custom_part seq) ->
			[make_heading Html5.h1 label (part_conv ~spanify:true order) !!"part" attr (write_seq seq)]

		| Part (label, order, Appendix) ->
			[make_heading Html5.h1 label (part_conv ~spanify:true order) !!"part" attr (write_name Name_appendix)]

		| Section (label, order, location, level, Custom_section seq) ->
			[make_sectional level label (section_conv ~spanify:true location order) attr (write_seq seq)]

		| Section (label, order, location, level, Bibliography) ->
			let title = make_sectional level label (section_conv ~spanify:true location order) attr (write_name Name_bibliography) in
			let bibs = match bibs with
				| []   -> []
				| _::_ -> [Html5.ol ~a:[a_class [!!"bibs"]] (List.map write_bib bibs)] in
			title :: bibs

		| Section (label, order, location, level, Notes) ->
			let title = make_sectional level label (section_conv ~spanify:true location order) attr (write_name Name_notes) in
			let notes = match notes with
				| []   -> []
				| _::_ -> [Html5.ol ~a:[a_class [!!"notes"]] (List.map write_note notes)] in
			title :: notes

		| Section (label, order, location, level, Toc) ->
			let title = make_sectional level label (section_conv ~spanify:true location order) attr (write_name Name_toc) in
			let entries = List.filter_map write_toc_entry toc in
			let toc_xhtml = match entries with
				| []   -> []
				| _::_ -> [Html5.ul ~a:[a_class [!!"toc"]] entries] in
			title :: toc_xhtml


	and write_custom data maybe_seq frag classname attr formatter =
		let (env, label, triple) = match data with
			| `Anonymous (env, label)	-> (env, label, (None, None, maybe write_seq maybe_seq))
			| `Unnumbered (env, label)	-> (env, label, (Some (write_name (Name_custom env)), None, maybe write_seq maybe_seq))
			| `Numbered (env, label, order) -> (env, label, (Some (write_name (Name_custom env)), Some order, maybe write_seq maybe_seq)) in
		let title = match formatter triple with
			| [] -> []
			| xs -> [Html5.h1 ~a:[a_class [classname ^ "_head"]] xs] in
		let content = title @ [Html5.div ~a:[a_class [classname ^ "_body"]] (write_frag frag)] in
		Html5.div ~a:[a_id (make_label label); a_class (classname :: (classname ^ "_env_"  ^ env) :: attr)] content


	and write_wrapper wrapper blk classname attr name =
		let wrapper_content = match write_block ~wrapped:true blk with
			| [b] -> b
			| _   -> failwith "write_wrapper" in
		let (length, label, caption_content) = match wrapper with
			| Wrapper.Ordered (label, order, maybe_seq) ->
				let headcore = (write_name name) @ (wrapper_conv ~prespace:true order) in
				begin match maybe_seq with
					| Some seq -> ("long", label, [Html5.h1 ~a:[a_class [!!"caption_head"]] (headcore @ [pcdata ":"]); Html5.p ~a:[a_class [!!"caption_body"]] (write_seq seq)])
					| None	   -> ("short", label, [Html5.h1 ~a:[a_class [!!"caption_head"]] ([pcdata "("] @ headcore @ [pcdata ")"])])
				end
			| Wrapper.Unordered (label, seq) ->
				("long", label, [Html5.p ~a:[a_class [!!"caption_body"]] (write_seq seq)]) in
		let caption = Html5.div ~a:[a_class [!!"caption"; "caption_" ^^ length]] [Html5.div ~a:[a_class [!!"caption_aux"]] caption_content] in
		Html5.div ~a:[a_id (make_label label); a_class (!!"wrapper" :: !!"floatable" :: classname :: attr)] [wrapper_content; caption]


	(************************************************************************)
	(*	{4 Writers for ghost elements}					*)
	(************************************************************************)

	and write_note note =
		Html5.li ~a:[a_id (make_label note.Note.label); a_class [!!"note"]]
			[
			Html5.span ~a:[a_class [!!"note_head"]] (pcdata "(" :: (note_conv note.Note.order) @ [pcdata ")"]);
			Html5.div ~a:[a_class [!!"note_body"]] (write_frag note.Note.content);
			]


	and write_bib bib =
		Html5.li ~a:[a_id (make_label bib.Bib.label); a_class [!!"bib"]]
			[
			Html5.span ~a:[a_class [!!"bib_head"]] (pcdata "[" :: (bib_conv bib.Bib.order) @ [pcdata "]"]);
			Html5.p ~a:[a_class [!!"bib_body"]]
				[
				Html5.span ~a:[a_class [!!"bib_author"]] (write_seq bib.Bib.author);
				Html5.span ~a:[a_class [!!"bib_title"]] (write_seq bib.Bib.title);
				Html5.span ~a:[a_class [!!"bib_resource"]] (write_seq bib.Bib.resource);
				]
			]


	and write_toc_entry sec =
		let make_toc_entry label classname orderlst content =
			Some (Html5.li ~a:[a_class [!!"item"; classname]] [make_internal_link label (orderlst @ content)])
		in match sec with
			| Part (label, order, Custom_part seq) ->
				make_toc_entry label (class_of_level 0) (part_conv ~spanify:true order) (Obj.magic (write_seq seq))
			| Part (label, order, Appendix) ->
				make_toc_entry label (class_of_level 0) (part_conv ~spanify:true order) (Obj.magic (write_name Name_appendix))
			| Section (label, order, location, level, Custom_section seq) ->
				make_toc_entry label (class_of_level (level :> int)) (section_conv ~spanify:true location order) (Obj.magic (write_seq seq))
			| Section (label, order, location, level, Bibliography) ->
				make_toc_entry label (class_of_level (level :> int)) (section_conv ~spanify:true location order) (Obj.magic (write_name Name_bibliography))
			| Section (label, order, location, level, Notes) ->
				make_toc_entry label (class_of_level (level :> int)) (section_conv ~spanify:true location order) (Obj.magic (write_name Name_notes))
			| Section (label, order, location, level, Toc) ->
				make_toc_entry label (class_of_level (level :> int)) (section_conv ~spanify:true location order) (Obj.magic (write_name Name_toc))


	in Html5.div ~a:[a_class (opts.prefix :: (List.map (!!) opts.base_classes) @ opts.extra_classes)] (write_frag content)


(********************************************************************************)
(**	{3 Conversion of invalid documents}					*)
(********************************************************************************)

let from_invalid ?(invalid_options = default_invalid_options) doc =

	let opts = invalid_options in

	let (!!) =
		let prefix = opts.prefix ^ "_" in
		fun str -> prefix ^ str in

	let write_error ((maybe_error_context, _, _) as error) =
		let context = match maybe_error_context with
			| Some error_context ->
				let line_number = error_context.Error.error_line_number
				and line_before = error_context.Error.error_line_before
				and line_actual = error_context.Error.error_line_actual
				and line_after = error_context.Error.error_line_after in
				let show_line classname delta line =
					Html5.li ~a:[a_class [classname]]
						[
						Html5.span ~a:[a_class [!!"error_linenum"]] [pcdata (sprintf "%03d" (line_number + delta))];
						Html5.span ~a:[a_class [!!"error_linestr"]] [pcdata line]
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
					Html5.h1 ~a:[a_class [!!"error_head"]] [pcdata (sprintf "Error in line %d:" line_number)];
					Html5.ul ~a:[a_class [!!"error_lines"]] (List.hd lines :: List.tl lines);
					]
			| None ->
				[Html5.h1 ~a:[a_class [!!"error_head"]] [pcdata "Global error:"]] in
		let explanation_doc = Valid.make
			~content:[Block.paragraph (Explanations.explain error)]
			~bibs:[]
			~notes:[]
			~toc:[]
			~labels:(Hashtbl.create 0)
			~customs:(Hashtbl.create 0)
			~links:(Hashtbl.create 0)
			~images:(Hashtbl.create 0) in
		let valid_options = {default_valid_options with prefix = opts.prefix; base_classes = ["error_msg"]} in
		let explanation_out = from_valid ~valid_options ~link_dict:(Hashtbl.create 0) ~image_dict:(Hashtbl.create 0) explanation_doc in
		Html5.li ~a:[a_class [!!"error"]] (context @ [explanation_out]) in
	let errors = List.map write_error doc in
	Html5.div ~a:[a_class (opts.prefix :: (List.map (!!) opts.base_classes) @ (List.map (!!) opts.extra_classes))] [ul ~a:[a_class [!!"errors"]] errors]
end

