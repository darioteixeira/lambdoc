(********************************************************************************)
(*	Implementation file for Document_postprocess.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Postprocessing on a document AST.  These functions convert
	a document AST into a proper, final, ambivalent document.
*)

open ExtString
open Lambdoc_core
open Basic
open Ast.M


(********************************************************************************)
(**	{2 Functions and values}						*)
(********************************************************************************)

(********************************************************************************)
(**	{3 Helper sub-functions}						*)
(********************************************************************************)

(**	This subfunction returns the language used for syntax highlighting.
	It invokes the utility function from the Code module, and adds an
	error if the language is unknown.
*)
let get_language errors comm = function
	| None ->
		None
	| Some "" ->
		None
	| Some other ->
		try
			Some (Code.lang_of_string other)
		with
			Invalid_argument x ->
				let msg = Error.Invalid_language (comm.comm_tag, x)
				in	DynArray.add errors (comm.comm_linenum, msg);
					None


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
	and possible errors are also returned.  Note that many of the internal
	functions have explicit type annotations.  While these are not required
	by the language, they make error messages far more comprehensible in
	a context where polymorphic variants are heavily used.
*)
let process_document feature_map document_ast =


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
	and add_toc_entry blk =
		DynArray.add toc blk in


	(************************************************************************)
	(* Checkers for operators and commands.					*)
	(************************************************************************)

	let checker feature success msg_maker linenum =
		let super_feature = (feature :> Features.manuscript_feature_t) in
		if Features.check_feature super_feature feature_map
		then 
			success ()
		else
			(let (what, desc) = Features.describe_feature super_feature in
			let msg = msg_maker (what, desc)
			in DynArray.add errors (linenum, msg);
			None) in


	let check_comm ?maybe_subpaged ?maybe_wrapped feature comm elem =
		let success () =
			Permissions.check_command_feature ?maybe_subpaged ?maybe_wrapped errors comm feature;
			elem ()
		and msg_maker (what, desc) = Error.Invalid_command_feature (what, desc)
		and linenum = comm.comm_linenum
		in checker feature success msg_maker linenum


	and check_op feature op elem =
		let msg_maker (what, desc) = Error.Invalid_operator_feature (what, desc)
		and linenum = op.op_linenum
		in checker feature elem msg_maker linenum in


	(************************************************************************)
	(* Postprocessing functions for mathematics.				*)
	(************************************************************************)

	let convert_mathtex constructor linenum txt =
		try
			Some (constructor (Math.from_mathtex txt))
		with
			Math.Invalid_mathtex ->
				DynArray.add errors (linenum, Error.Invalid_mathtex txt);
				None


	and convert_mathml constructor linenum txt =
		try
			Some (constructor (Math.from_mathml txt))
		with
			Math.Invalid_mathml ->
				DynArray.add errors (linenum, Error.Invalid_mathml txt);
				None in


	(************************************************************************)
	(* Postprocessing functions for inline context.				*)
	(************************************************************************)

	let rec convert_super_seq seq =
		ExtList.List.filter_map convert_super_node seq


	and convert_nonlink_seq seq =
		ExtList.List.filter_map convert_nonlink_node seq


	and convert_super_node = function

		| #Ast.M.nonlink_node_t as node ->
			(convert_nonlink_node node :> (Node.M.super_node_t, _) Node.M.t option)

		| #Ast.M.link_node_t as node ->
			(convert_link_node node :> (Node.M.super_node_t, _) Node.M.t option)


	and convert_nonlink_node = function

		| `AST_plain (op, txt) ->
			let elem () = Some (Node.M.plain txt)
			in check_op `Feature_plain op elem

		| `AST_entity (op, txt) ->
			let elem () = Some (Node.M.entity txt)
			in check_op `Feature_entity op elem

		| `AST_mathtex_inl (op, txt) ->
			let elem () = convert_mathtex Node.M.math op.op_linenum txt
			in check_op `Feature_mathtex_inl op elem

		| `AST_mathml_inl (op, txt) ->
			let elem () = convert_mathml Node.M.math op.op_linenum txt
			in check_op `Feature_mathml_inl op elem

		| `AST_bold (comm, seq) ->
			let elem () = Some (Node.M.bold (convert_super_seq seq))
			in check_comm `Feature_bold comm elem

		| `AST_emph (comm, seq) ->
			let elem () = Some (Node.M.emph (convert_super_seq seq))
			in check_comm `Feature_emph comm elem

		| `AST_mono (comm, seq) ->
			let elem () = Some (Node.M.mono (convert_super_seq seq))
			in check_comm `Feature_mono comm elem

		| `AST_caps (comm, seq) ->
			let elem () = Some (Node.M.caps (convert_super_seq seq))
			in check_comm `Feature_caps comm elem

		| `AST_thru (comm, seq) ->
			let elem () = Some (Node.M.thru (convert_super_seq seq))
			in check_comm `Feature_thru comm elem

		| `AST_sup (comm, seq) ->
			let elem () = Some (Node.M.sup (convert_super_seq seq))
			in check_comm `Feature_sup comm elem

		| `AST_sub (comm, seq) ->
			let elem () = Some (Node.M.sub (convert_super_seq seq))
			in check_comm `Feature_sub comm elem

		| `AST_mbox (comm, seq) ->
			let elem () = Some (Node.M.mbox (convert_super_seq seq))
			in check_comm `Feature_mbox comm elem


	and convert_link_node = function

		| `AST_link (comm, lnk, seq) ->
			let elem () = Some (Node.M.link lnk (convert_nonlink_seq seq))
			in check_comm `Feature_link comm elem

		| `AST_see (comm, label) ->
			let elem () =
				let target_checker = function
					| Target.Note_target _	-> `Valid_target
					| _			-> `Wrong_target Error.Target_note
				in add_reference target_checker comm label;
				Some (Node.M.see label)
			in check_comm `Feature_see comm elem

		| `AST_cite (comm, label) ->
			let elem () =
				let target_checker = function
					| Target.Bib_target _	-> `Valid_target
					| _			-> `Wrong_target Error.Target_bib
				in add_reference target_checker comm label;
				Some (Node.M.cite label)
			in check_comm `Feature_cite comm elem

		| `AST_ref (comm, label) ->
			let elem () =
				let target_checker = function
					| Target.Visible_target (Target.Part_target `None_given)	-> `Empty_target
					| Target.Visible_target (Target.Section_target (_, `None_given))-> `Empty_target
					| Target.Visible_target _					-> `Valid_target
					| _								-> `Wrong_target Error.Target_label
				in add_reference target_checker comm label;
				Some (Node.M.ref label)
			in check_comm `Feature_ref comm elem

		| `AST_sref (comm, label) ->
			let elem () =
				let target_checker = function
					| Target.Visible_target (Target.Part_target `None_given)	-> `Empty_target
					| Target.Visible_target (Target.Section_target (_, `None_given))-> `Empty_target
					| Target.Visible_target _					-> `Valid_target
					| _								-> `Wrong_target Error.Target_label
				in add_reference target_checker comm label;
				Some (Node.M.sref label)
			in check_comm `Feature_sref comm elem

		| `AST_mref (comm, label, seq) ->
			let elem () =
				let target_checker = function
					| Target.Visible_target _	-> `Valid_target
					| _				-> `Wrong_target Error.Target_label
				in add_reference target_checker comm label;
				Some (Node.M.mref label (convert_nonlink_seq seq))
			in check_comm `Feature_mref comm elem in


	(************************************************************************)
	(* Postprocessing functions for tabular environment.			*)
	(************************************************************************)

	let convert_tabular comm tab =
		let tcols = match comm.comm_secondary with
			| None		-> [| |]
			| Some thing	-> Array.map (get_column errors comm) (Array.of_list (String.explode thing)) in

		let num_columns = Array.length tcols in

		let convert_row (op, row) =
			(if List.length row <> num_columns
			then	let msg = Error.Invalid_column_number (comm.comm_linenum, List.length row, num_columns)
				in DynArray.add errors (op.op_linenum, msg));
			match row with
				| []		-> invalid_arg "Parser has given us an empty tabular row"
				| hd::tl	-> Tabular.make_row (fplus convert_super_seq hd tl) in

		let convert_group (maybe_comm, rows) =
			let () = match maybe_comm with
				| Some comm	-> ()
				| None		-> ()
			in match rows with
				| []		-> invalid_arg "Parser has given us an empty tabular group"
				| hd::tl	-> fplus convert_row hd tl in

		let thead = match tab.thead with
			| Some grp	-> Some (convert_group grp)
			| None		-> None

		and tfoot = match tab.tfoot with
			| Some grp	-> Some (convert_group grp)
			| None		-> None

		in match tab.tbodies with
			| []		-> invalid_arg "Parser has given us an empty tabular body"
			| hd::tl	-> Tabular.make tcols ?thead ?tfoot (fplus convert_group hd tl) in


	(************************************************************************)
	(* Postprocessing functions for document blocks.			*)
	(************************************************************************)

	let rec convert_super_frag ~subpaged frag =
		ExtList.List.filter_map (convert_super_block ~subpaged) frag


	and convert_nestable_frag ~subpaged frag =
		ExtList.List.filter_map (convert_nestable_block ~subpaged) frag


	and convert_paragraph_frag frag =
		ExtList.List.filter_map convert_paragraph_block frag


	and convert_item_frag ~subpaged = function
		| []		-> invalid_arg "Parser has given us an empty list!"
		| hd::tl	-> fplus (convert_item_block ~subpaged) hd tl


	and convert_super_block ~subpaged = function

		| #Ast.M.top_block_t as node ->
			(convert_top_block ~subpaged node :> (Block.M.super_block_t, _) Block.M.t option)

		| #Ast.M.nestable_block_t as node ->
			(convert_nestable_block ~subpaged node :> (Block.M.super_block_t, _) Block.M.t option)


	and convert_top_block ~subpaged = function

		| #Ast.M.heading_block_t as blk ->
			(convert_heading_block ~subpaged blk :> (Block.M.top_block_t, _) Block.M.t option)

		| `AST_title (level, comm, seq) ->
			let elem () = Some (Block.M.title level (convert_super_seq seq))
			and feature = match level with
				| `Level1 -> `Feature_title1
				| `Level2 -> `Feature_title2
			in check_comm feature comm elem

		| `AST_abstract (comm, frag) ->
			let elem () = Some (Block.M.abstract (convert_paragraph_frag frag))
			in check_comm `Feature_abstract comm elem

		| `AST_rule comm ->
			let elem () = Some (Block.M.rule ())
			in check_comm `Feature_rule comm elem


	and convert_heading_block ~subpaged = function

		| `AST_part (comm, seq) ->
			let elem () =
				let order = match comm.comm_order with
					| None		-> Order.auto_ordinal part_counter
					| Some ""	-> Order.none ()
					| Some other	-> Order.user_ordinal other in
				let label = make_label comm (Target.part_target order) in
				let block = Block.M.part label order (convert_super_seq seq) in
				let () = if not subpaged then add_toc_entry block
				in Some block
			in check_comm ~maybe_subpaged:(Some subpaged) `Feature_part comm elem

		| `AST_section (level, comm, seq) ->
			let elem () =
				let (counter, location) =
					if !appendixed
					then (appendix_counter, `Appendixed)
					else (section_counter, `Mainbody) in
				let order = match comm.comm_order with
					| None		-> Order.auto_hierarchical level counter
					| Some ""	-> Order.none ()
					| Some other	-> Order.user_hierarchical level other in
				let label = make_label comm (Target.section_target location order) in
				let block = Block.M.section label order location level (convert_super_seq seq) in
				let () = if not subpaged then add_toc_entry block
				in Some block
			and feature = match level with
				| `Level1 -> `Feature_section1
				| `Level2 -> `Feature_section2
				| `Level3 -> `Feature_section3
			in check_comm ~maybe_subpaged:(Some subpaged) feature comm elem

		| `AST_appendix comm ->
			let elem () =
				let order = Order.none () in
				let label = make_label comm (Target.part_target order) in
				let block = Block.M.appendix label in
				let () = if not subpaged then add_toc_entry block in
				let () = appendixed := true
				in Some block
			in check_comm ~maybe_subpaged:(Some subpaged) `Feature_appendix comm elem

		| `AST_bibliography comm ->
			convert_preset_sectional ~tocable:true ~subpaged Block.M.bibliography `Feature_bibliography comm

		| `AST_notes comm ->
			convert_preset_sectional ~tocable:true ~subpaged Block.M.notes `Feature_notes comm 

		| `AST_toc comm ->
			convert_preset_sectional ~tocable:false ~subpaged Block.M.toc `Feature_toc comm


	and convert_preset_sectional ~tocable ~subpaged cons feature comm = 
		let elem () =
			let order = Order.none () in
			let label = make_label comm (Target.section_target `Mainbody order) in
			let block = cons label in
			let () = if tocable && not subpaged then add_toc_entry block
			in Some block
		in check_comm ~maybe_subpaged:(Some subpaged) `Feature_notes comm elem


	and convert_nestable_block ~subpaged = function

		| #Ast.M.paragraph_block_t as blk ->
			(convert_paragraph_block blk :> (Block.M.nestable_block_t, _) Block.M.t option)

		| #Ast.M.itemize_block_t as blk ->
			(convert_itemize_block ~subpaged blk :> (Block.M.nestable_block_t, _) Block.M.t option)

		| #Ast.M.enumerate_block_t as blk ->
			(convert_enumerate_block ~subpaged blk :> (Block.M.nestable_block_t, _) Block.M.t option)

		| #Ast.M.quote_block_t as blk ->
			(convert_quote_block ~subpaged blk :> (Block.M.nestable_block_t, _) Block.M.t option)

		| #Ast.M.mathtex_block_t as blk ->
			(convert_mathtex_block blk :> (Block.M.nestable_block_t, _) Block.M.t option)

		| #Ast.M.mathml_block_t as blk ->
			(convert_mathml_block blk :> (Block.M.nestable_block_t, _) Block.M.t option)

		| #Ast.M.code_block_t as blk ->
			(convert_code_block blk :> (Block.M.nestable_block_t, _) Block.M.t option)

		| #Ast.M.verbatim_block_t as blk ->
			(convert_verbatim_block blk :> (Block.M.nestable_block_t, _) Block.M.t option)

		| #Ast.M.tabular_block_t as blk ->
			(convert_tabular_block blk :> (Block.M.nestable_block_t, _) Block.M.t option)

		| #Ast.M.bitmap_block_t as blk ->
			(convert_bitmap_block blk :> (Block.M.nestable_block_t, _) Block.M.t option)

		| #Ast.M.subpage_block_t as blk ->
			(convert_subpage_block blk :> (Block.M.nestable_block_t, _) Block.M.t option)

		| `AST_equation (comm, cap, blk) ->
			let elem () =
				let maybe_wrapper = convert_wrapper comm equation_counter Target.equation_target cap subpaged
				and maybe_equation = convert_equation_block blk
				in match (maybe_wrapper, maybe_equation) with
					| (Some wrapper, Some equation)	-> Some (Block.M.equation wrapper equation)
					| _				-> None
			in check_comm ~maybe_subpaged:(Some subpaged) `Feature_equation comm elem

		| `AST_printout (comm, cap, blk) ->
			let elem () =
				let maybe_wrapper = convert_wrapper comm printout_counter Target.printout_target cap subpaged
				and maybe_printout = convert_printout_block blk
				in match (maybe_wrapper, maybe_printout) with
					| (Some wrapper, Some printout)	-> Some (Block.M.printout wrapper printout)
					| _				-> None
			in check_comm ~maybe_subpaged:(Some subpaged) `Feature_printout comm elem

		| `AST_table (comm, cap, blk) ->
			let elem () =
				let maybe_wrapper = convert_wrapper comm table_counter Target.table_target cap subpaged
				and maybe_table = convert_table_block blk
				in match (maybe_wrapper, maybe_table) with
					| (Some wrapper, Some table)	-> Some (Block.M.table wrapper table)
					| _				-> None
			in check_comm ~maybe_subpaged:(Some subpaged) `Feature_table comm elem

		| `AST_figure (comm, cap, blk) ->
			let elem () =
				let maybe_wrapper = convert_wrapper comm figure_counter Target.figure_target cap subpaged
				and maybe_figure = convert_figure_block blk
				in match (maybe_wrapper, maybe_figure) with
					| (Some wrapper, Some figure)	-> Some (Block.M.figure wrapper figure)
					| _				-> None
			in check_comm ~maybe_subpaged:(Some subpaged) `Feature_figure comm elem

		| `AST_bib (comm, title, author, resource) ->
			let elem () =
				let order = Order.auto_ordinal bib_counter in
				let label = make_label comm (Target.bib_target order)
				and title = convert_bib_title_block title
				and author = convert_bib_author_block author
				and resource = convert_bib_resource_block resource
				in match (title, author, resource) with
					| (Some title, Some author, Some resource) ->
						let bib =
							{
							Bib.label = label;
							Bib.order = order;
							Bib.title = title;
							Bib.author = author;
							Bib.resource = resource;
							}
						in	DynArray.add bibs bib;
							None
					| _ ->
						None
			in check_comm `Feature_bib comm elem

		| `AST_note (comm, frag) ->
			let elem () =
				let order = Order.auto_ordinal note_counter in
				let label = make_label comm (Target.note_target order) in
				let note =
					{
					Note.label = label;
					Note.order = order;
					Note.content = convert_nestable_frag subpaged frag;
					}
				in	DynArray.add notes note;
					None
			in check_comm `Feature_note comm elem


	and convert_paragraph_block = function
		| `AST_paragraph (op, seq) ->
			let elem () = Some (Block.M.paragraph (convert_super_seq seq))
			in check_op `Feature_paragraph op elem


	and convert_itemize_block ~subpaged = function
		| `AST_itemize (comm, items) ->
			let elem () =
				let bullet = Extra.parse_for_itemize errors comm comm.comm_extra
				in Some (Block.M.itemize bullet (convert_item_frag subpaged items))
			in check_comm `Feature_itemize comm elem


	and convert_enumerate_block ~subpaged = function
		| `AST_enumerate (comm, items) ->
			let elem () =
				let numbering = Extra.parse_for_enumerate errors comm comm.comm_extra
				in Some (Block.M.enumerate numbering (convert_item_frag subpaged items))
			in check_comm `Feature_enumerate comm elem


	and convert_quote_block ~subpaged = function
		| `AST_quote (comm, frag) ->
			let elem () =
				let alignment = Extra.parse_for_quote errors comm comm.comm_extra
				in Some (Block.M.quote alignment (convert_nestable_frag subpaged frag))
			in check_comm `Feature_quote comm elem


	and convert_mathtex_block = function
		| `AST_mathtex_blk (comm, txt) ->
			let elem () =
				let alignment = Extra.parse_for_mathtex errors comm comm.comm_extra
				in convert_mathtex (Block.M.math alignment) comm.comm_linenum txt
			in check_comm `Feature_mathtex_blk comm elem


	and convert_mathml_block = function
		| `AST_mathml_blk (comm, txt) ->
			let elem () =
				let alignment = Extra.parse_for_mathml errors comm comm.comm_extra
				in convert_mathml (Block.M.math alignment) comm.comm_linenum txt
			in check_comm `Feature_mathml_blk comm elem


	and convert_code_block = function
		| `AST_code (comm, txt) ->
			let elem () =
				let lang = get_language errors comm comm.comm_secondary in
				let highlight = Code.from_string lang txt
				and alignment = Extra.parse_for_code errors comm comm.comm_extra
				in Some (Block.M.code alignment highlight)
			in check_comm `Feature_code comm elem


	and convert_verbatim_block = function
		| `AST_verbatim (comm, txt) ->
			let elem () =
				let alignment = Extra.parse_for_verbatim errors comm comm.comm_extra
				in Some (Block.M.verbatim alignment txt)
			in check_comm `Feature_verbatim comm elem


	and convert_tabular_block = function
		| `AST_tabular (comm, tab) ->
			let elem () =
				let alignment = Extra.parse_for_tabular errors comm comm.comm_extra
				in Some (Block.M.tabular alignment (convert_tabular comm tab))
			in check_comm `Feature_tabular comm elem


	and convert_bitmap_block = function
		| `AST_bitmap (comm, alias) ->
			let elem () =
				let alignment = Extra.parse_for_bitmap errors comm comm.comm_extra
				in Some (Block.M.bitmap alignment alias)
			in check_comm `Feature_bitmap comm elem


	and convert_subpage_block = function
		| `AST_subpage (comm, subpage) ->
			let elem () =
				let alignment = Extra.parse_for_subpage errors comm comm.comm_extra
				in Some (Block.M.subpage alignment (convert_super_frag true subpage))
			in check_comm `Feature_subpage comm elem


	and convert_bib_title_block = function
		| `AST_bib_title (comm, seq) ->
			let elem () = Some (convert_super_seq seq)
			in check_comm `Feature_bib_title comm elem


	and convert_bib_author_block = function
		| `AST_bib_author (comm, seq) ->
			let elem () = Some (convert_super_seq seq)
			in check_comm `Feature_bib_author comm elem


	and convert_bib_resource_block = function
		| `AST_bib_resource (comm, seq) ->
			let elem () = Some (convert_super_seq seq)
			in check_comm `Feature_bib_resource comm elem


	and convert_equation_block = function
		| #Ast.M.mathtex_block_t as blk ->
			(convert_mathtex_block blk :> (Block.M.equation_block_t, _) Block.M.t option)
		| #Ast.M.mathml_block_t as blk ->
			(convert_mathml_block blk :> (Block.M.equation_block_t, _) Block.M.t option)


	and convert_printout_block = function
		| #Ast.M.code_block_t as blk ->
			(convert_code_block blk :> (Block.M.printout_block_t, _) Block.M.t option)


	and convert_table_block = function
		| #Ast.M.tabular_block_t as blk ->
			(convert_tabular_block blk :> (Block.M.table_block_t, _) Block.M.t option)


	and convert_figure_block = function
		| #Ast.M.bitmap_block_t as blk ->
			(convert_bitmap_block blk :> (Block.M.figure_block_t, _) Block.M.t option)
		| #Ast.M.verbatim_block_t as blk ->
			(convert_verbatim_block blk :> (Block.M.figure_block_t, _) Block.M.t option)
		| #Ast.M.subpage_block_t as blk ->
			(convert_subpage_block blk :> (Block.M.figure_block_t, _) Block.M.t option)


	and convert_caption_block = function
		| `AST_caption (comm, seq) ->
			let elem () = Some (convert_super_seq seq)
			in check_comm `Feature_caption comm elem


	and convert_item_block ~subpaged = function
		| `AST_item (comm, frag) ->
			convert_nestable_frag subpaged frag


	and convert_wrapper comm counter target_maker caption_block subpaged =
		let order = match comm.comm_order with
			| None		-> Order.auto_ordinal counter
			| Some thing	-> Order.user_ordinal thing in
		let label = make_label comm (target_maker order) in
		let maybe_caption = convert_caption_block caption_block
		in match (label, order, maybe_caption) with
			| (label, order, Some caption)	-> Some (label, order, caption)
			| _				-> None in


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

	let contents = convert_super_frag false document_ast in
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

let process_manuscript ?deny_list ?accept_list ?default source document_ast =
	let feature_map = Features.load_manuscript_features ?deny_list ?accept_list ?default () in
	let (contents, bibs, notes, toc, labelmap, errors) = process_document feature_map document_ast in
	if List.length errors = 0
	then
		Ambivalent.make_valid_manuscript contents bibs notes toc labelmap
	else
		let sorted_errors = sort_errors (collate_errors source errors)
		in Ambivalent.make_invalid_manuscript sorted_errors


let process_composition ?deny_list ?accept_list ?default source document_ast =
	let feature_map = Features.load_composition_features ?deny_list ?accept_list ?default () in
	let (contents, _, _, _, _, errors) = process_document feature_map document_ast in
	if List.length errors = 0
	then
		let composition = Convert.convert_to_composition contents
		in Ambivalent.make_valid_composition composition
	else
		let sorted_errors = sort_errors (collate_errors source errors)
		in Ambivalent.make_invalid_composition sorted_errors

