(********************************************************************************)
(*	Postprocess.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Postprocessing on a document Ast.  These functions convert
	a document AST into a proper, final, ambivalent document.
*)

open ExtString
open ExtList
open Lambdoc_core
open Basic
open Ast


(********************************************************************************)
(**	{2 Functions and values}						*)
(********************************************************************************)

(********************************************************************************)
(**	{3 Helper sub-functions}						*)
(********************************************************************************)

(**	This subfunction returns the column floater and weight associated
	with a column specifier.
*)
let get_column errors comm spec =
	try
		Tabular.column_of_specifier spec
	with
		Tabular.Invalid_column_specifier spec ->
			let msg = Error.Invalid_column_specifier (comm.comm_tag, spec)
			in	DynArray.add errors (comm.comm_linenum, msg);
				(Tabular.Center, Tabular.Normal)


(********************************************************************************)
(**	{3 Low-level processing functions}					*)
(********************************************************************************)

(**	Processes an AST as provided by the parser, producing the corresponding
	document.  In addition, a label dictionary, bibliography entries, notes,
	and possible errors are also returned.  Note that because Ocaml does not
	yet support true GADTs, this function has to rely on Obj.magic.
*)
let process_document classnames idiosyncrasies document_ast =


	(************************************************************************)
	(* Declaration of the mutable values used in the function.		*)
	(************************************************************************)

	let errors = DynArray.create ()
	and bibs = DynArray.create ()
	and notes = DynArray.create ()
	and references = DynArray.create ()
	and toc = DynArray.create ()
        and labelmap = Labelmap.create ()
	and part_counter = Order.make_ordinal_counter ()
	and section_counter = Order.make_hierarchy_counter ()
	and appendix_counter = Order.make_hierarchy_counter ()
	and printout_counter = Order.make_ordinal_counter ()
	and equation_counter = Order.make_ordinal_counter ()
	and figure_counter = Order.make_ordinal_counter ()
	and table_counter = Order.make_ordinal_counter ()
	and bib_counter = Order.make_ordinal_counter ()
	and note_counter = Order.make_ordinal_counter ()
        and auto_label_counter = ref 0
	and appendixed = ref false in


	(************************************************************************)
	(* Helper sub-functions.						*)
	(************************************************************************)

	(*	This subfunction creates a new label.  It checks whether the user explicitly
		provide a label (in which case we use the `User_label variant), or if no
		label was defined (in which case we automatically assign a label using the
		`Auto_label variant).
	*)
	let make_label comm target =
		match comm.comm_label with
		| Some thing ->
			let new_label = `User_label thing in
			(if Labelmap.mem labelmap new_label
			then DynArray.add errors (comm.comm_linenum, (Error.Duplicate_label (comm.comm_tag, thing)))
			else Labelmap.add labelmap new_label target);
			new_label
		| None ->
			incr auto_label_counter;
			`Auto_label (string_of_int !auto_label_counter)


	(*	Adds a new reference to the dictionary.
	*)
	and add_reference target_checker comm label =
		DynArray.add references (target_checker, comm, label)


	(*	Adds a new TOC entry.
	*)
	and add_toc_entry heading =
		DynArray.add toc heading in


	(*	Checker for commands.
	*)

	let check_comm ?maybe_subpaged ?maybe_wrapped feature comm elem =
		let super_feature = (feature :> Features.manuscript_feature_t) in
		if Idiosyncrasies.check_feature super_feature idiosyncrasies
		then 
			(Permissions.check_feature ?maybe_subpaged ?maybe_wrapped errors comm feature;
			elem ())
		else
			let msg = Error.Invalid_feature (comm.comm_tag, Features.describe super_feature)
			in	DynArray.add errors (comm.comm_linenum, msg);
				None in


	(************************************************************************)
	(* Postprocessing functions for mathematics.				*)
	(************************************************************************)

	let convert_mathtex constructor comm mathtex =
		try
			let mathml = Blahcaml.safe_mathml_from_tex mathtex
			in Some (constructor (Math.from_both mathtex mathml))
		with _ ->
			let msg = Error.Invalid_mathtex (comm.comm_tag, mathtex) in
			DynArray.add errors (comm.comm_linenum, msg);
			None


	and convert_mathml constructor comm mathml =
		try
			let mathml = Blahcaml.sanitize_mathml mathml
			in Some (constructor (Math.from_mathml mathml))
		with _ ->
			let msg = Error.Invalid_mathml (comm.comm_tag, mathml) in
			DynArray.add errors (comm.comm_linenum, msg);
			None in


	(************************************************************************)
	(* Postprocessing functions for inline context.				*)
	(************************************************************************)

	let rec convert_inline is_link inline = match (is_link, inline) with

		| (_, (comm, Ast.Plain txt)) ->
			let elem () = Some (Inline.plain txt)
			in check_comm `Feature_plain comm elem

		| (_, (comm, Ast.Entity ent)) ->
			let elem () = match Entity.code_point ent with
				| `Okay num  -> Some (Inline.entity num)
				| `Error msg -> DynArray.add errors (comm.comm_linenum, msg); None
			in check_comm `Feature_entity comm elem

		| (_, (comm, Ast.Linebreak)) ->
			let elem () = Some (Inline.linebreak ())
			in check_comm `Feature_linebreak comm elem

		| (_, (comm, Ast.Mathtex_inl txt)) ->
			let elem () = convert_mathtex Inline.math comm txt
			in check_comm `Feature_mathtex_inl comm elem

		| (_, (comm, Ast.Mathml_inl txt)) ->
			let elem () = convert_mathml Inline.math comm txt
			in check_comm `Feature_mathml_inl comm elem

		| (x, (comm, Ast.Bold seq)) ->
			let elem () = Some (Inline.bold (List.filter_map (convert_inline x) seq))
			in check_comm `Feature_bold comm elem

		| (x, (comm, Ast.Emph seq)) ->
			let elem () = Some (Inline.emph (List.filter_map (convert_inline x) seq))
			in check_comm `Feature_emph comm elem

		| (x, (comm, Ast.Mono seq)) ->
			let elem () = Some (Inline.mono (List.filter_map (convert_inline x) seq))
			in check_comm `Feature_mono comm elem

		| (x, (comm, Ast.Caps seq)) ->
			let elem () = Some (Inline.caps (List.filter_map (convert_inline x) seq))
			in check_comm `Feature_caps comm elem

		| (x, (comm, Ast.Thru seq)) ->
			let elem () = Some (Inline.thru (List.filter_map (convert_inline x) seq))
			in check_comm `Feature_thru comm elem

		| (x, (comm, Ast.Sup seq)) ->
			let elem () = Some (Inline.sup (List.filter_map (convert_inline x) seq))
			in check_comm `Feature_sup comm elem

		| (x, (comm, Ast.Sub seq)) ->
			let elem () = Some (Inline.sub (List.filter_map (convert_inline x) seq))
			in check_comm `Feature_sub comm elem

		| (x, (comm, Ast.Mbox seq)) ->
			let elem () = Some (Inline.mbox (List.filter_map (convert_inline x) seq))
			in check_comm `Feature_mbox comm elem

		| (false, (comm, Ast.Link (lnk, []))) ->
			let elem () = Some (Inline.link lnk None)
			in check_comm `Feature_link comm elem

		| (false, (comm, Ast.Link (lnk, seq))) ->
			let elem () = Some (Inline.link lnk (Some (Obj.magic (List.filter_map (convert_inline true) seq))))
			in check_comm `Feature_link comm elem

		| (false, (comm, Ast.See label)) ->
			let elem () =
				let target_checker = function
					| Target.Note_target _	-> `Valid_target
					| _			-> `Wrong_target Error.Target_note
				in add_reference target_checker comm label;
				Some (Inline.see label)
			in check_comm `Feature_see comm elem

		| (false, (comm, Ast.Cite label)) ->
			let elem () =
				let target_checker = function
					| Target.Bib_target _	-> `Valid_target
					| _			-> `Wrong_target Error.Target_bib
				in add_reference target_checker comm label;
				Some (Inline.cite label)
			in check_comm `Feature_cite comm elem

		| (false, (comm, Ast.Ref label)) ->
			let elem () =
				let target_checker = function
					| Target.Visible_target (Target.Part_target `None_given)	-> `Empty_target
					| Target.Visible_target (Target.Section_target (_, `None_given))-> `Empty_target
					| Target.Visible_target _					-> `Valid_target
					| _								-> `Wrong_target Error.Target_label
				in add_reference target_checker comm label;
				Some (Inline.ref label)
			in check_comm `Feature_ref comm elem

		| (false, (comm, Ast.Sref label)) ->
			let elem () =
				let target_checker = function
					| Target.Visible_target (Target.Part_target `None_given)	-> `Empty_target
					| Target.Visible_target (Target.Section_target (_, `None_given))-> `Empty_target
					| Target.Visible_target _					-> `Valid_target
					| _								-> `Wrong_target Error.Target_label
				in add_reference target_checker comm label;
				Some (Inline.sref label)
			in check_comm `Feature_sref comm elem

		| (false, (comm, Ast.Mref (label, seq))) ->
			let elem () =
				let target_checker = function
					| Target.Visible_target _	-> `Valid_target
					| _				-> `Wrong_target Error.Target_label
				in add_reference target_checker comm label;
				Some (Inline.mref label (Obj.magic (List.filter_map (convert_inline true) seq)))
			in check_comm `Feature_mref comm elem

		| (_, (comm, _)) ->
			let msg = Error.Nested_link comm.comm_tag
			in DynArray.add errors (comm.comm_linenum, msg);
			None in


	let convert_seq seq =
		Obj.magic (List.filter_map (convert_inline false) seq) in


	(************************************************************************)
	(* Postprocessing functions for tabular environment.			*)
	(************************************************************************)

	let convert_tabular comm tcols tab =
		let columns = Array.map (get_column errors comm) (Array.of_list (String.explode tcols)) in

		let num_columns = Array.length columns in

		let convert_row (comm, row) =
			(if List.length row <> num_columns
			then	let msg = Error.Invalid_column_number (comm.comm_tag, comm.comm_linenum, List.length row, num_columns)
				in DynArray.add errors (comm.comm_linenum, msg));
			match row with
				| []		-> invalid_arg "Parser has given us an empty tabular row"
				| hd::tl	-> Tabular.make_row (fplus convert_seq hd tl) in

		let convert_group (maybe_comm, rows) =
			let () = match maybe_comm with
				| Some comm	-> ()
				| None		-> ()
			in match rows with
				| []		-> invalid_arg "Parser has given us an empty tabular group"
				| hd::tl	-> Tabular.make_group (fplus convert_row hd tl) in

		let thead = maybe convert_group tab.thead

		and tfoot = maybe convert_group tab.tfoot

		in match tab.tbodies with
			| []		-> invalid_arg "Parser has given us an empty tabular body"
			| hd::tl	-> Tabular.make_tabular columns ?thead ?tfoot (fplus convert_group hd tl) in


	(************************************************************************)
	(* Postprocessing functions for document blocks.			*)
	(************************************************************************)

	let rec convert_block ~subpaged allow_nestable allow_top allowed_blk block = match (allow_nestable, allow_top, allowed_blk, block) with

		| (_, _, `Paragraph_blk, (comm, Ast.Paragraph seq))
		| (_, _, `Any_blk, (comm, Ast.Paragraph seq)) ->
			let elem () = Some (Block.paragraph Indentation.Indent_default (convert_seq seq))
			in check_comm `Feature_paragraph comm elem

		| (x, _, `Any_blk, (comm, Ast.Itemize [])) ->
			let msg = Error.Empty_listing comm.comm_tag
			in DynArray.add errors (comm.comm_linenum, msg);
			None

		| (x, _, `Any_blk, (comm, Ast.Itemize frags)) ->
			let elem () =
				let bullet = Extra.parse_for_itemize errors comm
				and new_frags = List.filter_map (convert_item_frag ~subpaged x) frags
				in match new_frags with
					| hd::tl	-> Some (Block.itemize bullet (hd, tl))
					| []		-> None
			in check_comm `Feature_itemize comm elem

		| (x, _, `Any_blk, (comm, Ast.Enumerate [])) ->
			let msg = Error.Empty_listing comm.comm_tag
			in DynArray.add errors (comm.comm_linenum, msg);
			None

		| (x, _, `Any_blk, (comm, Ast.Enumerate frags)) ->
			let elem () =
				let numbering = Extra.parse_for_enumerate errors comm
				and new_frags = List.filter_map (convert_item_frag ~subpaged x) frags
				in match new_frags with
					| hd::tl	-> Some (Block.enumerate numbering (hd, tl))
					| []		-> None
			in check_comm `Feature_enumerate comm elem

		| (x, _, `Any_blk, (comm, Ast.Description [])) ->
			let msg = Error.Empty_listing comm.comm_tag
			in DynArray.add errors (comm.comm_linenum, msg);
			None

		| (x, _, `Any_blk, (comm, Ast.Description frags)) ->
			let elem () =
				let new_frags = List.filter_map (convert_describe_frag ~subpaged x) frags
				in match new_frags with
					| hd::tl	-> Some (Block.description (hd, tl))
					| []		-> None
			in check_comm `Feature_description comm elem

		| (_, _, `Any_blk, (comm, Ast.Quote frag)) ->
			let elem () =
				let new_frag = List.filter_map (convert_block ~subpaged false false `Any_blk) frag
				in Some (Block.quote (Obj.magic new_frag))
			in check_comm `Feature_quote comm elem

		| (_, _, `Any_blk, (comm, Ast.Pullquote frag)) ->
			let elem () =
				let alignment = Extra.parse_for_pullquote errors comm
				and new_frag = List.filter_map (convert_block ~subpaged false false `Any_blk) frag
				in Some (Block.pullquote alignment (Obj.magic new_frag))
			in check_comm `Feature_pullquote comm elem

		| (_, _, `Any_blk, (comm, Ast.Boxout (maybe_seq, frag))) ->
			let elem () =
				let (alignment, maybe_classname) = Extra.parse_for_boxout ~classnames errors comm
				and new_frag = List.filter_map (convert_block ~subpaged false false `Any_blk) frag
				and seq = maybe convert_seq maybe_seq
				in Some (Block.boxout alignment maybe_classname seq (Obj.magic new_frag))
			in check_comm `Feature_boxout comm elem

		| (_, _, `Equation_blk, (comm, Ast.Mathtex_blk txt))
		| (_, _, `Any_blk, (comm, Ast.Mathtex_blk txt)) ->
			let elem () =
				let alignment = Extra.parse_for_mathtex errors comm
				in convert_mathtex (Block.math alignment) comm txt
			in check_comm `Feature_mathtex_blk comm elem

		| (_, _, `Equation_blk, (comm, Ast.Mathml_blk txt))
		| (_, _, `Any_blk, (comm, Ast.Mathml_blk txt)) ->
			let elem () =
				let alignment = Extra.parse_for_mathml errors comm
				in convert_mathml (Block.math alignment) comm txt
			in check_comm `Feature_mathml_blk comm elem

		| (_, _, `Printout_blk, (comm, Ast.Code txt))
		| (_, _, `Any_blk, (comm, Ast.Code txt)) ->
			let elem () =
				let (alignment, lang, linenums, zebra) = Extra.parse_for_code errors comm in
				let hilite = Camlhighlight_parser.from_string lang txt in
				let code = Code.make lang linenums zebra hilite
				in Some (Block.code alignment code)
			in check_comm `Feature_code comm elem

		| (_, _, `Table_blk, (comm, Ast.Tabular (tcols, tab)))
		| (_, _, `Any_blk, (comm, Ast.Tabular (tcols, tab))) ->
			let elem () =
				let alignment = Extra.parse_for_tabular errors comm
				in Some (Block.tabular alignment (convert_tabular comm tcols tab))
			in check_comm `Feature_tabular comm elem

		| (_, _, `Figure_blk, (comm, Ast.Verbatim txt))
		| (_, _, `Any_blk, (comm, Ast.Verbatim txt)) ->
			let elem () =
				let alignment = Extra.parse_for_verbatim errors comm
				in Some (Block.verbatim alignment txt)
			in check_comm `Feature_verbatim comm elem

		| (_, _, `Figure_blk, (comm, Ast.Bitmap (alias, alt)))
		| (_, _, `Any_blk, (comm, Ast.Bitmap (alias, alt))) ->
			let elem () =
				let (alignment, frame, width) = Extra.parse_for_bitmap errors comm in
				let image = Image.make frame width alias alt
				in Some (Block.bitmap alignment image)
			in check_comm `Feature_bitmap comm elem

		| (_, _, `Figure_blk, (comm, Ast.Subpage frag))
		| (_, _, `Any_blk, (comm, Ast.Subpage frag)) ->
			let elem () =
				let alignment = Extra.parse_for_subpage errors comm
				and new_frag = List.filter_map (convert_block ~subpaged:true true true `Any_blk) frag
				in Some (Block.subpage alignment (Obj.magic new_frag))
			in check_comm `Feature_subpage comm elem

		| (true, _, `Any_blk, (comm, Ast.Equation (caption, blk))) ->
			let elem () =
				let maybe_wrapper = convert_wrapper comm equation_counter Target.equation caption subpaged
				and maybe_equation = Obj.magic (convert_block ~subpaged true true `Equation_blk blk)
				in match (maybe_wrapper, maybe_equation) with
					| (Some wrapper, Some equation)	-> Some (Block.equation wrapper equation)
					| _				-> None
			in check_comm ~maybe_subpaged:(Some subpaged) `Feature_equation comm elem

		| (true, _, `Any_blk, (comm, Ast.Printout (caption, blk))) ->
			let elem () =
				let maybe_wrapper = convert_wrapper comm printout_counter Target.printout caption subpaged
				and maybe_printout = Obj.magic (convert_block ~subpaged true true `Printout_blk blk)
				in match (maybe_wrapper, maybe_printout) with
					| (Some wrapper, Some printout)	-> Some (Block.printout wrapper printout)
					| _				-> None
			in check_comm ~maybe_subpaged:(Some subpaged) `Feature_printout comm elem

		| (true, _, `Any_blk, (comm, Ast.Table (caption, blk))) ->
			let elem () =
				let maybe_wrapper = convert_wrapper comm table_counter Target.table caption subpaged
				and maybe_table = Obj.magic (convert_block ~subpaged true true `Table_blk blk)
				in match (maybe_wrapper, maybe_table) with
					| (Some wrapper, Some table)	-> Some (Block.table wrapper table)
					| _				-> None
			in check_comm ~maybe_subpaged:(Some subpaged) `Feature_table comm elem

		| (true, _, `Any_blk, (comm, Ast.Figure (caption, blk))) ->
			let elem () =
				let maybe_wrapper = convert_wrapper comm figure_counter Target.figure caption subpaged
				and maybe_figure = Obj.magic (convert_block ~subpaged true true `Figure_blk blk)
				in match (maybe_wrapper, maybe_figure) with
					| (Some wrapper, Some figure)	-> Some (Block.figure wrapper figure)
					| _				-> None
			in check_comm ~maybe_subpaged:(Some subpaged) `Feature_figure comm elem

		| (true, true, `Any_blk, (comm, Ast.Part seq)) ->
			let elem () =
				let order = match comm.comm_order with
					| None		-> Order.auto_ordinal part_counter
					| Some ""	-> Order.none ()
					| Some other	-> Order.user_ordinal other in
				let label = make_label comm (Target.part order) in
				let heading = Heading.part label order (convert_seq seq) in
				let block = Block.heading heading in
				let () = if not subpaged then add_toc_entry heading
				in Some block
			in check_comm ~maybe_subpaged:(Some subpaged) `Feature_part comm elem

		| (true, true, `Any_blk, (comm, Ast.Section (level, seq))) ->
			let elem () =
				let (counter, location) =
					if !appendixed
					then (appendix_counter, `Appendixed)
					else (section_counter, `Mainbody) in
				let order = match comm.comm_order with
					| None		-> Order.auto_hierarchical level counter
					| Some ""	-> Order.none ()
					| Some other	-> Order.user_hierarchical level other in
				let label = make_label comm (Target.section location order) in
				let heading = Heading.section label order location level (convert_seq seq) in
				let block = Block.heading heading in
				let () = if not subpaged then add_toc_entry heading
				in Some block
			and feature = match level with
				| `Level1 -> `Feature_section1
				| `Level2 -> `Feature_section2
				| `Level3 -> `Feature_section3
			in check_comm ~maybe_subpaged:(Some subpaged) feature comm elem

		| (true, true, `Any_blk, (comm, Ast.Appendix)) ->
			let elem () =
				let order = Order.none () in
				let label = make_label comm (Target.part order) in
				let heading = Heading.appendix label in
				let block = Block.heading heading in
				let () = if not subpaged then add_toc_entry heading in
				let () = appendixed := true
				in Some block
			in check_comm ~maybe_subpaged:(Some subpaged) `Feature_appendix comm elem

		| (true, true, `Any_blk, (comm, Ast.Bibliography)) ->
			convert_preset_sectional ~tocable:true ~subpaged Heading.bibliography `Feature_bibliography comm

		| (true, true, `Any_blk, (comm, Ast.Notes)) ->
			convert_preset_sectional ~tocable:true ~subpaged Heading.notes `Feature_notes comm 

		| (true, true, `Any_blk, (comm, Ast.Toc)) ->
			convert_preset_sectional ~tocable:false ~subpaged Heading.toc `Feature_toc comm

		| (true, true, `Any_blk, (comm, Ast.Title (level, seq))) ->
			let elem () = Some (Block.title level (convert_seq seq))
			and feature = match level with
				| `Level1 -> `Feature_title1
				| `Level2 -> `Feature_title2
			in check_comm feature comm elem

		| (true, true, `Any_blk, (comm, Ast.Abstract frag)) ->
			let elem () =
				let new_frag = List.filter_map (convert_block true true `Paragraph_blk ~subpaged) frag
				in Some (Block.abstract (Obj.magic new_frag))
			in check_comm `Feature_abstract comm elem

		| (true, true, `Any_blk, (comm, Ast.Rule)) ->
			let elem () = Some (Block.rule ())
			in check_comm `Feature_rule comm elem

		| (_, _, `Any_blk, (comm, Ast.Bib bib)) ->
			let elem () =
				let (author_comm, author_seq) = bib.author
				and (title_comm, title_seq) = bib.title
				and (resource_comm, resource_seq) = bib.resource in
				let order = Order.auto_ordinal bib_counter in
				let label = make_label comm (Target.bib order)
				and author =
					let elem () = Some (convert_seq author_seq)
					in check_comm `Feature_bib_author author_comm elem
				and title =
					let elem () = Some (convert_seq title_seq)
					in check_comm `Feature_bib_title title_comm elem
				and resource =
					let elem () = Some (convert_seq resource_seq)
					in check_comm `Feature_bib_resource resource_comm elem
				in match (author, title, resource) with
					| (Some author, Some title, Some resource) ->
						let bib = Bib.bib label order author title resource
						in	DynArray.add bibs bib;
							None
					| _ ->
						None
			in check_comm `Feature_bib comm elem

		| (_, _, `Any_blk, (comm, Ast.Note frag)) ->
			let elem () =
				let order = Order.auto_ordinal note_counter in
				let label = make_label comm (Target.note order) in
				let new_frag = List.filter_map (convert_block true false `Any_blk ~subpaged) frag in
				let note = Note.note label order new_frag
				in	DynArray.add notes note;
					None
			in check_comm `Feature_note comm elem

		| (_, _, x, (comm, _)) ->
			let msg = Error.Unexpected_block (comm.comm_tag, x)
			in DynArray.add errors (comm.comm_linenum, msg);
			None


	and convert_item_frag ~subpaged allow_nestable (comm, frag) =
		let elem () = Some (List.filter_map (Obj.magic (convert_block ~subpaged allow_nestable false `Any_blk)) frag)
		in check_comm `Feature_item comm elem


	and convert_describe_frag ~subpaged allow_nestable (comm, seq, frag) =
		let elem () =
			let new_seq = convert_seq seq
			and new_frag = List.filter_map (Obj.magic (convert_block ~subpaged allow_nestable false `Any_blk)) frag
			in Some (new_seq, new_frag)
		in check_comm `Feature_describe comm elem


	and convert_preset_sectional ~tocable ~subpaged cons feature comm = 
		let elem () =
			let order = Order.none () in
			let label = make_label comm (Target.section `Mainbody order) in
			let heading = cons label in
			let block = Block.heading heading in
			let () = if tocable && not subpaged then add_toc_entry heading
			in Some block
		in check_comm ~maybe_subpaged:(Some subpaged) `Feature_notes comm elem


	and convert_wrapper comm counter target_maker (caption_comm, caption_seq) subpaged =
		let order = match comm.comm_order with
			| None		-> Order.auto_ordinal counter
			| Some thing	-> Order.user_ordinal thing in
		let label = make_label comm (target_maker order) in
		let maybe_caption = 
			let elem () = Some (convert_seq caption_seq)
			in check_comm `Feature_caption caption_comm elem
		in match (label, order, maybe_caption) with
			| (label, order, Some caption)	-> Some (label, order, caption)
			| _				-> None in


	let convert_frag frag =
		List.filter_map (convert_block ~subpaged:false true true `Any_blk) frag in


	(************************************************************************)
	(* Function for filtering references.					*)
	(************************************************************************)

	let filter_references () =
		let filter_reference (target_checker, comm, label) =
			try
				let target = Labelmap.find labelmap (`User_label label) in
				match target_checker target with
				| `Valid_target ->
					()
				| `Empty_target ->
					let msg = Error.Empty_target (comm.comm_tag, label)
					in DynArray.add errors (comm.comm_linenum, msg)
				| `Wrong_target expected ->
					let suggestion = match target with
						| Target.Visible_target _	-> Error.Target_label
						| Target.Bib_target _		-> Error.Target_bib
						| Target.Note_target _		-> Error.Target_note in
					let msg = Error.Wrong_target (comm.comm_tag, expected, suggestion, label)
					in DynArray.add errors (comm.comm_linenum, msg)
			with
				Not_found ->
					let msg = Error.Absent_target (comm.comm_tag, label) in
					DynArray.add errors (comm.comm_linenum, msg)
		in
		DynArray.iter filter_reference references in


	(************************************************************************)
	(* Wrap-up.								*)
	(************************************************************************)

	let contents = convert_frag document_ast in
	let () = filter_references () in
	let res_bibs = DynArray.to_list bibs
	and res_notes = DynArray.to_list notes
	and res_toc = DynArray.to_list toc
	and res_errors = DynArray.to_list errors
	in (contents, res_bibs, res_notes, res_toc, labelmap, res_errors)


(********************************************************************************)
(**	{3 Error postprocessing functions}					*)
(********************************************************************************)

(**	Error collation function.  It takes a list of errors containing each an
	error message and an error line, and produces a proper error message
	where the actual source lines are displayed.
*)
let collate_errors source errors =
	let source_lines = Array.of_list (String.nsplit source "\n") in
	let format_error (error_linenum, error_msg) =
		let error_context =
			{
			Error.error_line_number = error_linenum;
			Error.error_line_before =
				if error_linenum >= 2
				then [source_lines.(error_linenum-2)]
				else [];
			Error.error_line_actual = source_lines.(error_linenum-1);
			Error.error_line_after =
				if error_linenum < (Array.length source_lines)
				then [source_lines.(error_linenum)]
				else []
			}
		in (error_context, error_msg)
	in List.map format_error errors


(**	Sorts the errors by line number.
*)
let sort_errors errors =
	let compare (a, _) (b, _) = Pervasives.compare a.Error.error_line_number b.Error.error_line_number
	in List.stable_sort compare errors


(********************************************************************************)
(**	{3 Top-level processing functions}					*)
(********************************************************************************)

let process_manuscript ?(classnames = []) ?accept_list ?deny_list ?default source document_ast =
	let idiosyncrasies = Idiosyncrasies.make_manuscript_idiosyncrasies ?accept_list ?deny_list ?default () in
	let (contents, bibs, notes, toc, labelmap, errors) = process_document classnames idiosyncrasies document_ast in
	if List.length errors = 0
	then
		Ambivalent.make_valid_manuscript contents bibs notes toc labelmap
	else
		let sorted_errors = sort_errors (collate_errors source errors)
		in Ambivalent.make_invalid_manuscript sorted_errors


let process_composition ?(classnames = []) ?accept_list ?deny_list ?default source document_ast =
	let idiosyncrasies = Idiosyncrasies.make_composition_idiosyncrasies ?accept_list ?deny_list ?default () in
	let (contents, _, _, _, _, errors) = process_document classnames idiosyncrasies document_ast in
	if List.length errors = 0
	then
		Ambivalent.make_valid_composition (Obj.magic contents)
	else
		let sorted_errors = sort_errors (collate_errors source errors)
		in Ambivalent.make_invalid_composition sorted_errors

