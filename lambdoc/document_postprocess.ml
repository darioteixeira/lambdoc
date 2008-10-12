(********************************************************************************)
(**	Postprocessing on a document AST.  These functions convert a document AST
	into a proper, final, ambivalent document.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open ExtString
open Document_basic
open Document_ast
open Document_ref
open Document_node
open Document_block
open Document_features
open Document_ghost
open Document_tabular
open Document_settings
open Document_error
open Document_features
open Document_permissions
open Document_math
open Document_ambivalent


(********************************************************************************)
(**	{2 Private functions}							*)
(********************************************************************************)

(**	Processes an AST as provided by the parser, producing the corresponding
	document.  In addition, a list of labels, bibliography entries, notes,
	and possible errors is also returned.  Note that many of the internal
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
        and labels = Hashtbl.create 50
        and auto_label_counter = ref 0
	and section_counter = ref 0
	and subsection_counter = ref 0
	and subsubsection_counter = ref 0
	and appendix_counter = ref 0
	and subappendix_counter = ref 0
	and subsubappendix_counter = ref 0
	and algorithm_counter = ref 0
	and equation_counter = ref 0
	and figure_counter = ref 0
	and table_counter = ref 0
	and bib_counter = ref 0
	and note_counter = ref 0
	and appendixed = ref false in


	(************************************************************************)
	(* Helper sub-functions.						*)
	(************************************************************************)

	(*	This subfunction returns the type of bullet associated with
		an itemize environment.  If no extra parameter is given, the
		bullet type is assumed to be the default.
	*)
	let get_bullet comm = function
		| None ->
			Bullet.Default
		| Some thing ->
			try
				Bullet.of_string thing
			with
				Bullet.Unknown_bullet_type x ->
					let msg = Error.Unknown_bullet_type (comm.Ast.comm_tag, x) in
					DynArray.add errors (comm.Ast.comm_linenum, msg);
					Bullet.Default


	(*	This subfunction returns the type of numbering associated with
		an enumerate environment.  If no extra parameter is given, the
		numbering type is assumed to be the default.
	*)
	and get_numbering comm = function
		| None ->
			Numbering.Default
		| Some thing ->
			try
				Numbering.of_string thing
			with
				Numbering.Unknown_numbering_type x ->
					let msg = Error.Unknown_numbering_type (comm.Ast.comm_tag, x) in
					DynArray.add errors (comm.Ast.comm_linenum, msg);
					Numbering.Default


	(**	This subfunction returns the type of alignment associated with
		a floater or quote environment.  If no extra parameter is given,
		the alignment type is assumed to be [Center].
	*)
	and get_alignment comm = function
		| None	->
			Alignment.Center
		| Some thing ->
			try
				Alignment.of_string thing
			with
				Alignment.Unknown_alignment_type x ->
					let msg = Error.Unknown_alignment_type (comm.Ast.comm_tag, x) in
					DynArray.add errors (comm.Ast.comm_linenum, msg);
					Alignment.Center


	(**	This function returns the column alignment and weight associated
		with a column specifier.
	*)
	and get_column comm spec =
		try
			Tabular.column_of_specifier spec
		with
			Tabular.Invalid_column_specifier spec ->
				let msg = Error.Invalid_column_specifier (comm.Ast.comm_tag, spec)
				in DynArray.add errors (comm.Ast.comm_linenum, msg);
				(Tabular.Center, Tabular.Normal) in


	(*	This subfunction creates a new label.  It checks whether the user explicitly
		provide a label (in which case we use the `User_label variant), or if no
		label was defined (in which case we automatically assign a label using the
		`Auto_label variant).
	*)
	let make_label comm order =
		match comm.Ast.comm_label with
		| Some thing ->
			let new_label = `User_label thing in
			(if Hashtbl.mem labels new_label
			then DynArray.add errors (comm.Ast.comm_linenum, (Error.Duplicate_label (comm.Ast.comm_tag, thing)))
			else Hashtbl.add labels new_label order);
			new_label
		| None ->
			incr auto_label_counter;
			`Auto_label (string_of_int !auto_label_counter)


	(*	This subfunction creates a new floater order.  It basically checks whether
		the ordering for the floater was explicitly provided by the user or if we
		must assign one automatically.  In the latter case, the figure will be issued
		an empty ordering if it has no caption.
	*)
	and make_floater_order comm counter =
		match comm.Ast.comm_order with
			| None ->
				incr counter;
				`Auto_order (Order.Numeric [!counter])
			| Some thing ->
				`User_order thing


	(*	This subfunction creates a new ghost order.  Since ghost elements do not
		accept user provided ordering, the order is always computed automatically.
	*)
	and make_ghost_order comm counter =
		incr counter;
		`Auto_order (Order.Numeric [!counter])


	(*	Adds a new reference to the dictionary.
	*)
	and add_reference target_checker comm label =
		DynArray.add references (target_checker, comm, label)


	(*	Adds a new TOC entry.
	*)
	and add_toc_entry = function
		| `Heading blk	-> DynArray.add toc blk
		| _		-> failwith "Oops: attempted to add non-heading entry into TOC" in


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


	let check_comm comm feature maybe_subpaged elem =
		let success () = Permissions.check_command_feature errors comm maybe_subpaged feature; elem ()
		and msg_maker (what, desc) = Error.Invalid_command_feature (what, desc)
		and linenum = comm.Ast.comm_linenum
		in checker feature success msg_maker linenum


	and check_op op feature elem =
		let msg_maker (what, desc) = Error.Invalid_command_feature (what, desc)
		and linenum = op.Ast.op_linenum
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


	and convert_textual_seq seq =
		ExtList.List.filter_map convert_textual_node seq


	and convert_textual_node : Ast.textual_node_t -> (Node.textual_node_t, _) Node.t option = function

		| `AST_plain (op, txt) ->
			let elem () = Some (Node.plain txt)
			in check_op op `Feature_plain elem

		| `AST_entity (op, txt) ->
			let elem () = Some (Node.entity txt)
			in check_op op `Feature_entity elem


	and convert_nonlink_node : Ast.nonlink_node_t -> (Node.nonlink_node_t, _) Node.t option = function

		| #Ast.textual_node_t as node ->
			((convert_textual_node node) :> (Node.nonlink_node_t, _) Node.t option)

		| `AST_mathtex_inl (op, txt) ->
			let elem () = convert_mathtex Node.math op.Ast.op_linenum txt
			in check_op op `Feature_mathtex_inl elem

		| `AST_mathml_inl (op, txt) ->
			let elem () = convert_mathml Node.math op.Ast.op_linenum txt
			in check_op op `Feature_mathml_inl elem

		| `AST_bold (comm, seq) ->
			let elem () = Some (Node.bold (convert_super_seq seq))
			in check_comm comm `Feature_bold None elem

		| `AST_emph (comm, seq) ->
			let elem () = Some (Node.emph (convert_super_seq seq))
			in check_comm comm `Feature_emph None elem

		| `AST_mono (comm, seq) ->
			let elem () = Some (Node.mono (convert_super_seq seq))
			in check_comm comm `Feature_mono None elem

		| `AST_caps (comm, seq) ->
			let elem () = Some (Node.caps (convert_super_seq seq))
			in check_comm comm `Feature_caps None elem

		| `AST_thru (comm, seq) ->
			let elem () = Some (Node.thru (convert_super_seq seq))
			in check_comm comm `Feature_thru None elem

		| `AST_sup (comm, seq) ->
			let elem () = Some (Node.sup (convert_super_seq seq))
			in check_comm comm `Feature_sup None elem

		| `AST_sub (comm, seq) ->
			let elem () = Some (Node.sub (convert_super_seq seq))
			in check_comm comm `Feature_sub None elem

		| `AST_box (comm, seq) ->
			let elem () = Some (Node.box (convert_super_seq seq))
			in check_comm comm `Feature_box None elem


	and convert_link_node : Ast.link_node_t -> (Node.link_node_t, _) Node.t option = function

		| `AST_link (comm, lnk, seq) ->
			let elem () = Some (Node.link lnk (convert_nonlink_seq seq))
			in check_comm comm `Feature_link None elem

		| `AST_see (comm, label) ->
			let elem () =
				let target_checker = function
					| Order.Note_order _	-> `Valid_target
					| _			-> `Wrong_target Error.Target_note
				in add_reference target_checker comm label;
				Some (Node.see label)
			in check_comm comm `Feature_see None elem

		| `AST_cite (comm, label) ->
			let elem () =
				let target_checker = function
					| Order.Bib_order _	-> `Valid_target
					| _			-> `Wrong_target Error.Target_bib
				in add_reference target_checker comm label;
				Some (Node.cite label)
			in check_comm comm `Feature_cite None elem

		| `AST_ref (comm, label) ->
			let elem () =
				let target_checker = function
					| Order.Block_order (Order.Body_sectional_order `None_order)
					| Order.Block_order (Order.Appendix_sectional_order `None_order)
					| Order.Block_order (Order.Preset_sectional_order `None_order)
					| Order.Block_order (Order.Floater_order (_, `None_order))	-> `Empty_target
					| Order.Block_order _						-> `Valid_target
					| _ -> `Wrong_target Error.Target_label
				in add_reference target_checker comm label;
				Some (Node.ref label)
			in check_comm comm `Feature_ref None elem

		| `AST_sref (comm, label) ->
			let elem () =
				let target_checker = function
					| Order.Block_order (Order.Body_sectional_order `None_order)
					| Order.Block_order (Order.Appendix_sectional_order `None_order)
					| Order.Block_order (Order.Preset_sectional_order `None_order)
					| Order.Block_order (Order.Floater_order (_, `None_order))	-> `Empty_target
					| Order.Block_order _						-> `Valid_target
					| _ -> `Wrong_target Error.Target_label
				in add_reference target_checker comm label;
				Some (Node.sref label)
			in check_comm comm `Feature_sref None elem

		| `AST_mref (comm, label, seq) ->
			let elem () =
				let target_checker = function
					| Order.Block_order _	-> `Valid_target
					| _			-> `Wrong_target Error.Target_label
				in add_reference target_checker comm label;
				Some (Node.mref label (convert_nonlink_seq seq))
			in check_comm comm `Feature_mref None elem


	and convert_super_node : Ast.super_node_t -> (Node.super_node_t, _) Node.t option = function

		| #Ast.nonlink_node_t as node ->
			(convert_nonlink_node node :> (Node.super_node_t, _) Node.t option)

		| #Ast.link_node_t as node ->
			(convert_link_node node :> (Node.super_node_t, _) Node.t option) in


	(************************************************************************)
	(* Postprocessing functions for tabular environment.			*)
	(************************************************************************)

	let convert_tabular comm tab =
		let tcols = match comm.Ast.comm_secondary with
			| None		-> [| |]
			| Some thing	-> Array.map (get_column comm) (Array.of_list (String.explode thing)) in

		let num_columns = Array.length tcols in

		let convert_row (op, row) =
			(if List.length row <> num_columns
			then	let msg = Error.Wrong_column_number (comm.Ast.comm_linenum, List.length row, num_columns)
				in DynArray.add errors (op.Ast.op_linenum, msg));
			match row with
				| []		-> failwith "Parser has given us an empty tabular row"
				| hd::tl	-> Tabular.make_row (fplus convert_super_seq hd tl) in

		let convert_group (maybe_comm, rows) =
			let () = match maybe_comm with
				| Some comm	-> ()
				| None		-> ()
			in match rows with
				| []		-> failwith "Parser has given us an empty tabular group"
				| hd::tl	-> fplus convert_row hd tl in

		let thead = match tab.Ast.thead with
			| Some grp	-> Some (convert_group grp)
			| None		-> None

		and tfoot = match tab.Ast.tfoot with
			| Some grp	-> Some (convert_group grp)
			| None		-> None

		in match tab.Ast.tbodies with
			| []		-> failwith "Parser has given us an empty tabular body"
			| hd::tl	-> Tabular.make tcols ?thead ?tfoot (fplus convert_group hd tl) in


	(************************************************************************)
	(* Postprocessing functions for document blocks.			*)
	(************************************************************************)

	let rec convert_super_frag subpaged frag =
		ExtList.List.filter_map (convert_super_block subpaged) frag


	and convert_nestable_frag subpaged frag =
		ExtList.List.filter_map (convert_nestable_block subpaged) frag


	and convert_paragraph_frag frag =
		ExtList.List.filter_map convert_paragraph_block frag


	and convert_item_frag subpaged = function
		| []		-> failwith "Parser has given us an empty list!"
		| hd::tl	-> fplus (convert_item_block subpaged) hd tl


	and convert_caption_block : Ast.caption_block_t -> (Node.super_node_t, _) Node.t list option = function

		| `AST_caption (comm, seq) ->
			let elem () = Some (convert_super_seq seq)
			in check_comm comm `Feature_caption None elem


	and convert_item_block : bool -> Ast.item_block_t -> (Block.nestable_block_t, _) Block.t list = function subpaged -> function
		| `AST_item (comm, frag) ->
			convert_nestable_frag subpaged frag


	and convert_paragraph_block : Ast.paragraph_block_t -> (Block.paragraph_block_t, _) Block.t option = function
		| `AST_paragraph (op, seq) ->
			let elem () = Some (Block.paragraph (convert_super_seq seq))
			in check_op op `Feature_paragraph elem


	and convert_itemize_block : bool -> Ast.itemize_block_t -> (Block.itemize_block_t, _) Block.t option = function subpaged -> function
		| `AST_itemize (comm, items) ->
			let elem () =
				let bullet = get_bullet comm comm.Ast.comm_extra
				in Some (Block.itemize bullet (convert_item_frag subpaged items))
			in check_comm comm `Feature_itemize None elem


	and convert_enumerate_block : bool -> Ast.enumerate_block_t -> (Block.enumerate_block_t, _) Block.t option = function subpaged -> function
		| `AST_enumerate (comm, items) ->
			let elem () =
				let numbering = get_numbering comm comm.Ast.comm_extra
				in Some (Block.enumerate numbering (convert_item_frag subpaged items))
			in check_comm comm `Feature_enumerate None elem


	and convert_quote_block : bool -> Ast.quote_block_t -> (Block.quote_block_t, _) Block.t option = function subpaged -> function
		| `AST_quote (comm, frag) ->
			let elem () =
				let alignment = get_alignment comm comm.Ast.comm_extra
				in Some (Block.quote alignment (convert_nestable_frag subpaged frag))
			in check_comm comm `Feature_quote None elem


	and convert_mathtex_block : Ast.mathtex_block_t -> (Block.math_block_t, _) Block.t option = function
		| `AST_mathtex_blk (comm, txt) ->
			let elem () =
				let alignment = get_alignment comm comm.Ast.comm_extra
				in convert_mathtex (Block.math alignment) comm.Ast.comm_linenum txt
			in check_comm comm `Feature_mathtex_blk None elem


	and convert_mathml_block : Ast.mathml_block_t -> (Block.math_block_t, _) Block.t option = function
		| `AST_mathml_blk (comm, txt) ->
			let elem () =
				let alignment = get_alignment comm comm.Ast.comm_extra
				in convert_mathml (Block.math alignment) comm.Ast.comm_linenum txt
			in check_comm comm `Feature_mathml_blk None elem


	and convert_code_block : Ast.code_block_t -> (Block.code_block_t, _) Block.t option = function
		| `AST_code (comm, seq) ->
			let elem () =
				let alignment = get_alignment comm comm.Ast.comm_extra
				and syntax = comm.Ast.comm_secondary
				in Some (Block.code alignment syntax (convert_textual_seq seq))
			in check_comm comm `Feature_code None elem


	and convert_verbatim_block : Ast.verbatim_block_t -> (Block.verbatim_block_t, _) Block.t option = function
		| `AST_verbatim (comm, seq) ->
			let elem () =
				let alignment = get_alignment comm comm.Ast.comm_extra
				in Some (Block.verbatim alignment (convert_textual_seq seq))
			in check_comm comm `Feature_verbatim None elem


	and convert_tabular_block : Ast.tabular_block_t -> (Block.tabular_block_t, _) Block.t option = function
		| `AST_tabular (comm, tab) ->
			let elem () =
				let alignment = get_alignment comm comm.Ast.comm_extra
				in Some (Block.tabular alignment (convert_tabular comm tab))
			in check_comm comm `Feature_tabular None elem


	and convert_image_block : Ast.image_block_t -> (Block.image_block_t, _) Block.t option = function
		| `AST_image (comm, alias) ->
			let elem () =
				let alignment = get_alignment comm comm.Ast.comm_extra
				in Some (Block.image alignment alias)
			in check_comm comm `Feature_image None elem


	and convert_subpage_block : Ast.subpage_block_t -> (Block.subpage_block_t, _) Block.t option = function
		| `AST_subpage (comm, subpage) ->
			let elem () =
				let alignment = get_alignment comm comm.Ast.comm_extra
				in Some (Block.subpage alignment (convert_super_frag true subpage))
			in check_comm comm `Feature_subpage None elem


	and convert_bib_title_block : Ast.bib_title_block_t -> (Node.super_node_t, _) Node.t list option = function

		| `AST_bib_title (comm, seq) ->
			let elem () = Some (convert_super_seq seq)
			in check_comm comm `Feature_bib_title None elem


	and convert_bib_author_block : Ast.bib_author_block_t -> (Node.super_node_t, _) Node.t list option = function

		| `AST_bib_author (comm, seq) ->
			let elem () = Some (convert_super_seq seq)
			in check_comm comm `Feature_bib_author None elem


	and convert_bib_resource_block : Ast.bib_resource_block_t -> (Node.super_node_t, _) Node.t list option = function

		| `AST_bib_resource (comm, seq) ->
			let elem () = Some (convert_super_seq seq)
			in check_comm comm `Feature_bib_resource None elem


	and convert_equation_block : Ast.equation_block_t -> Block.equation_block_t option = function
		| #Ast.mathtex_block_t as blk ->
			(convert_mathtex_block blk :> (Block.equation_block_t, _) Block.t option)
		| #Ast.mathml_block_t as blk ->
			(convert_mathml_block blk :> (Block.equation_block_t, _) Block.t option)


	and convert_algorithm_block : Ast.algorithm_block_t -> Block.algorithm_block_t option = function
		| #Ast.code_block_t as blk ->
			(convert_code_block blk :> (Block.algorithm_block_t, _) Block.t option)


	and convert_table_block : Ast.table_block_t -> Block.table_block_t option = function
		| #Ast.tabular_block_t as blk ->
			(convert_tabular_block blk :> (Block.table_block_t, _) Block.t option)


	and convert_figure_block : Ast.figure_block_t -> Block.figure_block_t option = function
		| #Ast.image_block_t as blk ->
			(convert_image_block blk :> (Block.figure_block_t, _) Block.t option)
		| #Ast.verbatim_block_t as blk ->
			(convert_verbatim_block blk :> (Block.figure_block_t, _) Block.t option)
		| #Ast.subpage_block_t as blk ->
			(convert_subpage_block blk :> (Block.figure_block_t, _) Block.t option)


	and convert_floater comm counter order_func cap =
		let order = make_floater_order comm counter in
		let label = make_label comm (order_func order) in
		let maybe_caption = convert_caption_block cap
		in match (label, order, maybe_caption) with
			| (label, order, Some caption)	-> Some (label, order, caption)
			| _				-> None


	and convert_nestable_block : bool -> Ast.nestable_block_t -> (Block.nestable_block_t, _) Block.t option = function subpaged -> function

		| #Ast.paragraph_block_t as blk ->
			(convert_paragraph_block blk :> (Block.nestable_block_t, _) Block.t option)

		| #Ast.itemize_block_t as blk ->
			(convert_itemize_block subpaged blk :> (Block.nestable_block_t, _) Block.t option)

		| #Ast.enumerate_block_t as blk ->
			(convert_enumerate_block subpaged blk :> (Block.nestable_block_t, _) Block.t option)

		| #Ast.quote_block_t as blk ->
			(convert_quote_block subpaged blk :> (Block.nestable_block_t, _) Block.t option)

		| #Ast.mathtex_block_t as blk ->
			(convert_mathtex_block blk :> (Block.nestable_block_t, _) Block.t option)

		| #Ast.mathml_block_t as blk ->
			(convert_mathml_block blk :> (Block.nestable_block_t, _) Block.t option)

		| #Ast.code_block_t as blk ->
			(convert_code_block blk :> (Block.nestable_block_t, _) Block.t option)

		| #Ast.verbatim_block_t as blk ->
			(convert_verbatim_block blk :> (Block.nestable_block_t, _) Block.t option)

		| #Ast.tabular_block_t as blk ->
			(convert_tabular_block blk :> (Block.nestable_block_t, _) Block.t option)

		| #Ast.image_block_t as blk ->
			(convert_image_block blk :> (Block.nestable_block_t, _) Block.t option)

		| #Ast.subpage_block_t as blk ->
			(convert_subpage_block blk :> (Block.nestable_block_t, _) Block.t option)

		| `AST_equation (comm, cap, eq) ->
			let elem () =
				let maybe_floater = convert_floater comm equation_counter Order.equation_order cap
				and maybe_equation = convert_equation_block eq
				in match (maybe_floater, maybe_equation) with
					| (Some floater, Some equation)		-> Some (Block.equation floater equation)
					| _					-> None
			in check_comm comm `Feature_equation (Some subpaged) elem

		| `AST_algorithm (comm, cap, alg) ->
			let elem () =
				let maybe_floater = convert_floater comm algorithm_counter Order.algorithm_order cap
				and maybe_algorithm = convert_algorithm_block alg
				in match (maybe_floater, maybe_algorithm) with
					| (Some floater, Some algorithm)	-> Some (Block.algorithm floater algorithm)
					| _					-> None
			in check_comm comm `Feature_algorithm (Some subpaged) elem

		| `AST_table (comm, cap, tab) ->
			let elem () =
				let maybe_floater = convert_floater comm table_counter Order.table_order cap
				and maybe_table = convert_table_block tab
				in match (maybe_floater, maybe_table) with
					| (Some floater, Some table)		-> Some (Block.table floater table)
					| _					-> None
			in check_comm comm `Feature_table (Some subpaged) elem

		| `AST_figure (comm, cap, fig) ->
			let elem () =
				let maybe_floater = convert_floater comm figure_counter Order.figure_order cap
				and maybe_figure = convert_figure_block fig
				in match (maybe_floater, maybe_figure) with
					| (Some floater, Some figure)		-> Some (Block.figure floater figure)
					| _					-> None
			in check_comm comm `Feature_figure (Some subpaged) elem

		| `AST_bib (comm, title, author, resource) ->
			let elem () =
				let order = make_ghost_order comm bib_counter in
				let label = make_label comm (Order.bib_order order)
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
						in DynArray.add bibs bib;
						None
					| _ ->
						None
			in check_comm comm `Feature_bib None elem

		| `AST_note (comm, frag) ->
			let elem () =
				let order = make_ghost_order comm note_counter in
				let label = make_label comm (Order.note_order order) in
				let note =
					{
					Note.label = label;
					Note.order = order;
					Note.content = convert_nestable_frag subpaged frag;
					}
				in DynArray.add notes note;
				None
			in check_comm comm `Feature_note None elem


	and convert_heading_block : bool -> Ast.heading_block_t -> (Block.top_block_t, _) Block.t option = function subpaged -> function

		| `AST_section (comm, seq) ->
			let elem () =
				let block =
					if !appendixed
					then
						let order = match comm.Ast.comm_order with
							| None ->
								(appendix_counter := !appendix_counter + 1;
								subappendix_counter := 0;
								subsubappendix_counter := 0;
								`Auto_order (Order.Appendic [!appendix_counter]))
							| Some "" ->
								`None_order
							| Some other ->
								`User_order other in
						let label = make_label comm (Order.appendix_sectional_order order)
						in Block.appendix label order (convert_super_seq seq)
					else
						let order = match comm.Ast.comm_order with
							| None ->
								(section_counter := !section_counter + 1;
								subsection_counter := 0;
								subsubsection_counter := 0;
								`Auto_order (Order.Numeric [!section_counter]))
							| Some "" ->
								`None_order
							| Some other ->
								`User_order other in
						let label = make_label comm (Order.body_sectional_order order)
						in Block.section label order (convert_super_seq seq)
				in (if not subpaged then add_toc_entry block);
				Some block
			in check_comm comm `Feature_section (Some subpaged) elem

		| `AST_subsection (comm, seq) ->
			let elem () =
				let block =
					if !appendixed
					then
						let order = match comm.Ast.comm_order with
							| None ->
								(subappendix_counter := !subappendix_counter + 1;
								subsubappendix_counter := 0;
								`Auto_order (Order.Appendic
											[!appendix_counter;
											!subappendix_counter]))
							| Some "" ->
								`None_order
							| Some other ->
								`User_order other in
						let label = make_label comm (Order.appendix_sectional_order order)
						in Block.subappendix label order (convert_super_seq seq)
					else
						let order = match comm.Ast.comm_order with
							| None ->
								(subsection_counter := !subsection_counter + 1;
								subsubsection_counter := 0;
								`Auto_order (Order.Numeric
										[!section_counter;
										!subsection_counter]))
							| Some "" ->
								`None_order
							| Some other ->
								`User_order other in
						let label = make_label comm (Order.body_sectional_order order)
						in Block.subsection label order (convert_super_seq seq)
				in (if not subpaged then add_toc_entry block);
				Some block
			in check_comm comm `Feature_subsection (Some subpaged) elem

		| `AST_subsubsection (comm, seq) ->
			let elem () =
				let block =
					if !appendixed
					then
						let order = match comm.Ast.comm_order with
							| None ->
								(subsubappendix_counter := !subsubappendix_counter + 1;
								`Auto_order (Order.Appendic
											[!appendix_counter;
											!subappendix_counter;
											!subsubappendix_counter]))
							| Some "" ->
								`None_order
							| Some other ->
								`User_order other in
						let label = make_label comm (Order.appendix_sectional_order order)
						in Block.subsubappendix label order (convert_super_seq seq)
					else
						let order = match comm.Ast.comm_order with
							| None ->
								(subsubsection_counter := !subsubsection_counter + 1;
								`Auto_order (Order.Numeric
											[!section_counter;
											!subsection_counter;
											!subsubsection_counter]))
							| Some "" ->
								`None_order
							| Some other ->
								`User_order other in
						let label = make_label comm (Order.body_sectional_order order)
						in Block.subsubsection label order (convert_super_seq seq)
				in (if not subpaged then add_toc_entry block);
				Some block
			in check_comm comm `Feature_subsubsection (Some subpaged) elem

		| `AST_toc comm ->
			let elem () =
				let order = `None_order in
				let label = make_label comm (Order.preset_sectional_order order)
				in Some (Block.toc label order)
			in check_comm comm `Feature_toc None elem

		| `AST_bibliography comm ->
			let elem () =
				let order = `None_order in
				let label = make_label comm (Order.preset_sectional_order order) in
				let block = Block.bibliography label order in
				(if not subpaged then add_toc_entry block);
				Some block
			in check_comm comm `Feature_bibliography None elem

		| `AST_notes comm ->
			let elem () =
				let order = `None_order in
				let label = make_label comm (Order.preset_sectional_order order) in
				let block = Block.notes label order in
				(if not subpaged then add_toc_entry block);
				Some block
			in check_comm comm `Feature_notes None elem


	and convert_top_block : bool -> Ast.top_block_t -> (Block.top_block_t, _) Block.t option = function subpaged -> function

		| `AST_heading heading ->
			(convert_heading_block subpaged heading :> (Block.top_block_t, _) Block.t option)

		| `AST_title (comm, seq) ->
			let elem () = Some (Block.title (convert_super_seq seq))
			in check_comm comm `Feature_title None elem

		| `AST_abstract (comm, frag) ->
			let elem () = Some (Block.abstract (convert_paragraph_frag frag))
			in check_comm comm `Feature_abstract None elem

		| `AST_rule comm ->
			let elem () = Some (Block.rule ())
			in check_comm comm `Feature_rule None elem

		| `AST_appendix comm ->
			let elem () =
				appendixed := true;
				None
			in check_comm comm `Feature_appendix None elem


	and convert_super_block : bool -> Ast.super_block_t -> (Block.super_block_t, _) Block.t option = function subpaged -> function

		| #Ast.top_block_t as node ->
			(convert_top_block subpaged node :> (Block.super_block_t, _) Block.t option)

		| #Ast.nestable_block_t as node ->
			(convert_nestable_block subpaged node :> (Block.super_block_t, _) Block.t option) in


	(************************************************************************)
	(* Reference filter function.						*)
	(************************************************************************)

	let filter_references () =
		let filter_reference (target_checker, comm, label) =
			try
				let target = Hashtbl.find labels (`User_label label) in
				match target_checker target with
				| `Valid_target ->
					()
				| `Empty_target ->
					let msg = Error.Empty_target (comm.Ast.comm_tag, label)
					in DynArray.add errors (comm.Ast.comm_linenum, msg)
				| `Wrong_target expected ->
					let suggestion = match target with
						| Order.Block_order _	-> Error.Target_label
						| Order.Bib_order _	-> Error.Target_bib
						| Order.Note_order _	-> Error.Target_note in
					let msg = Error.Wrong_target (comm.Ast.comm_tag, expected, suggestion, label)
					in DynArray.add errors (comm.Ast.comm_linenum, msg)
			with
				Not_found ->
					let msg = Error.Absent_target (comm.Ast.comm_tag, label) in
					DynArray.add errors (comm.Ast.comm_linenum, msg)
		in
		DynArray.iter filter_reference references in


	(************************************************************************)
	(* Wrap-up.								*)
	(************************************************************************)

	let contents = convert_super_frag false document_ast in
	filter_references ();
	let res_bibs = DynArray.to_list bibs
	and res_notes = DynArray.to_list notes
	and res_toc = DynArray.to_list toc
	and res_errors = DynArray.to_list errors
	in (contents, res_bibs, res_notes, res_toc, labels, res_errors)


(********************************************************************************)
(**	{2 Public functions}							*)
(********************************************************************************)


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
(**	{3 Processing functions}						*)
(********************************************************************************)

let process_manuscript ?deny_list ?accept_list ?default source document_ast =
	let feature_map = Features.load_manuscript_features ?deny_list ?accept_list ?default () in
	let (contents, bibs, notes, toc, labels, errors) = process_document feature_map document_ast in
	if List.length errors = 0
	then
		Ambivalent.make_valid_manuscript contents bibs notes toc labels
	else
		let sorted_errors = sort_errors (collate_errors source errors)
		in Ambivalent.make_invalid_manuscript sorted_errors


let process_composition ?deny_list ?accept_list ?default source document_ast =
	let feature_map = Features.load_composition_features ?deny_list ?accept_list ?default () in
	let (contents, _, _, _, _, errors) = process_document feature_map document_ast in
	if List.length errors = 0
	then
		let composition = Document_convert.convert_to_composition contents
		in Ambivalent.make_valid_composition composition
	else
		let sorted_errors = sort_errors (collate_errors source errors)
		in Ambivalent.make_invalid_composition sorted_errors

