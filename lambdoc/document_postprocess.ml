(********************************************************************************)
(**	Postprocessing on documents.

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
open Document_ghost
open Document_tabular
open Document_settings
open Document_error
open Document_math
open Document_ambivalent


(********************************************************************************)
(**	{2 Private helper modules}						*)
(********************************************************************************)


(********************************************************************************)
(**	{3 Permission module}							*)
(********************************************************************************)

(**	The [Permission] module declares the various permission classes for
	command parameters and provides functions to verify their compliance.
*)
module Permission =
struct
	(**	The type encoding the various kinds of available permissions.
	*)
	type t =
		| Optional		(** The parameter is optional but may not be empty. *)
		| Optional0		(** The parameter is optional and may be empty. *)
		| Mandatory		(** The parameter is mandatory and may not be empty. *)
		| Mandatory0		(** The parameter is mandatory but may be empty. *)
		| Forbidden		(** The parameter is forbidden, either empty or not. *)
		| Forbidden0		(** The parameter is forbidden, unless it is empty. *)


	(**	The following values/functions encode the predefined permissions for
		the various classes of commands.  Each permission class is a 4-tuple
		stating the individual permissions for the label, ordering, extra, and
		secondary parameters, respectively.  While most classes are constant,
		some of them are context-sensitive and are therefore functions.
	*)

	let forbidden_class =
		(Forbidden, Forbidden, Forbidden, Forbidden)

	let user_sectional_class subpaged =
		let perm_order = if subpaged then Mandatory0 else Forbidden0
		in (Optional, perm_order, Forbidden, Forbidden)

	let preset_sectional_class =
		(Optional, Forbidden, Forbidden, Forbidden)

	let math_class =
		(Forbidden, Forbidden, Forbidden, Mandatory)

	let tabular_class =
		(Forbidden, Forbidden, Forbidden, Mandatory)

	let listing_class =
		(Forbidden, Forbidden, Optional, Forbidden)

	let quote_class =
		(Forbidden, Forbidden, Optional, Forbidden)

	let floater_class perm_secondary subpaged captioned =
		let perm_order = match (subpaged, captioned) with
			| (true, true)		-> Mandatory
			| (true, false)		-> Mandatory0
			| (false, true)		-> Forbidden
			| (false, false)	-> Forbidden0
		in (Optional, perm_order, Optional, perm_secondary)

	let algorithm_class = floater_class Mandatory0

	let equation_class = floater_class Forbidden

	let figure_class = floater_class Mandatory

	let table_class = floater_class Forbidden

	let ghost_class = (Optional, Forbidden, Forbidden, Forbidden)


	(*	This function checks whether a parameter is valid given its
		associated permission.  It returns an optional value stating
		the reason why the parameter was deemed invalid.  A [None]
		result indicates the parameter is valid.
	*)
	let reason_why_invalid perm = function
		| Some "" -> (match perm with
			| Optional0
			| Mandatory0
			| Forbidden0	-> None
			| Optional
			| Mandatory	-> Some Error.Reason_is_empty_when_non_empty_mandatory
			| Forbidden	-> Some Error.Reason_is_empty_when_forbidden)
		| Some other -> (match perm with
			| Forbidden
			| Forbidden0	-> Some (Error.Reason_is_non_empty_when_forbidden other)
			| _		-> None)
		| None -> (match perm with
			| Mandatory0
			| Mandatory	-> Some Error.Reason_is_absent_when_mandatory
			| _		-> None)


	(*	This function goes through all the command parameters, checking
		each one individually for correctness.  Any errors found are
		added to the [errors] [DynArray].
	*)
	let check errors comm (perm_label, perm_order, perm_extra, perm_secondary) =

		(match reason_why_invalid perm_label comm.comm_label with
			| None ->
				()
			| Some reason ->
				let msg = Error.Bad_label_parameter (comm.comm_tag, reason) in
				DynArray.add errors (comm.comm_linenum, msg));

		(match reason_why_invalid perm_order comm.comm_order with
			| None ->
				()
			| Some reason ->
				let msg = Error.Bad_order_parameter (comm.comm_tag, reason) in
				DynArray.add errors (comm.comm_linenum, msg));

		(match reason_why_invalid perm_extra comm.comm_extra with
			| None ->
				()
			| Some reason ->
				let msg = Error.Bad_extra_parameter (comm.comm_tag, reason) in
				DynArray.add errors (comm.comm_linenum, msg));

		(match reason_why_invalid perm_secondary comm.comm_secondary with
			| None ->
				()
			| Some reason ->
				let msg = Error.Bad_secondary_parameter (comm.comm_tag, reason) in
				DynArray.add errors (comm.comm_linenum, msg))
end


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
let process_document document_ast =

	let errors = DynArray.create ()
	and bibs = DynArray.create ()
	and notes = DynArray.create ()
	and references = DynArray.create ()
	and toc = DynArray.create ()
        and labels = Hashtbl.create 50
	and settings = ref (Settings.make_default ())
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
					let msg = Error.Unknown_bullet_type (comm.comm_tag, x) in
					DynArray.add errors (comm.comm_linenum, msg);
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
					let msg = Error.Unknown_numbering_type (comm.comm_tag, x) in
					DynArray.add errors (comm.comm_linenum, msg);
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
					let msg = Error.Unknown_alignment_type (comm.comm_tag, x) in
					DynArray.add errors (comm.comm_linenum, msg);
					Alignment.Center


	(**	This function returns the column alignment and weight associated
		with a column specifier.
	*)
	and get_column comm spec =
		try
			Tabular.column_of_specifier spec
		with
			Tabular.Invalid_column_specifier spec ->
				let msg = Error.Invalid_column_specifier (comm.comm_tag, spec)
				in DynArray.add errors (comm.comm_linenum, msg);
				(Tabular.Center, Tabular.Normal) in


	(*	This subfunction creates a new label.  It checks whether the user explicitly
		provide a label (in which case we use the `User_label variant), or if no
		label was defined (in which case we automatically assign a label using the
		`Auto_label variant).
	*)
	let make_label comm order =
		match comm.comm_label with
		| Some thing ->
			let new_label = `User_label thing in
			(if Hashtbl.mem labels new_label
			then DynArray.add errors (comm.comm_linenum, (Error.Duplicate_label (comm.comm_tag, thing)))
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
		match comm.comm_order with
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
		| _		-> failwith "Oops: attempted to add non-heading entry into TOC"


	(*	Adds a new setting.
	*)
	and add_setting comm key value =
		match key with
			| "section_name" ->
				settings := Settings.set_section_name !settings value
			| "appendix_name" ->
				settings := Settings.set_appendix_name !settings value
			| "algorithm_name" ->
				settings := Settings.set_algorithm_name !settings value
			| "equation_name" ->
				settings := Settings.set_equation_name !settings value
			| "figure_name" ->
				settings := Settings.set_figure_name !settings value
			| "table_name" ->
				settings := Settings.set_table_name !settings value
			| "bibliography_name" ->
				settings := Settings.set_bibliography_name !settings value
			| "notes_name" ->
				settings := Settings.set_notes_name !settings value
			| "toc_name" ->
				settings := Settings.set_toc_name !settings value
			| "default_bullet" ->
				settings := Settings.set_default_bullet !settings (get_bullet comm (Some value))
			| "default_numbering" ->
				settings := Settings.set_default_numbering !settings (get_numbering comm (Some value))
			| _ ->
				DynArray.add errors (comm.comm_linenum, Error.Unknown_setting key) in


	(************************************************************************)
	(* Checkers for naked, operator, and command nodes.			*)
	(************************************************************************)

	let check_naked feature elem = elem ()

	and check_op op feature elem = elem ()

	and check_comm comm feature elem = elem () in


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


	and convert_super_node : Document_ast.super_node_t -> (Node.super_node_t, _) Node.t option = function

		| `AST_nonlink_node node ->
			(convert_nonlink_node node :> (Node.super_node_t, _) Node.t option)

		| `AST_link_node node ->
			(convert_link_node node :> (Node.super_node_t, _) Node.t option)


	and convert_nonlink_node : Document_ast.nonlink_node_t -> (Node.nonlink_node_t, _) Node.t option = function

		| `AST_textual node ->
			((convert_textual_node node) :> (Node.nonlink_node_t, _) Node.t option)

		| `AST_mathtex_inl (op, txt) ->
			let elem () = convert_mathtex Node.math op.op_linenum txt
			in check_op op `Feature_mathtex_inl elem

		| `AST_mathml_inl (op, txt) ->
			let elem () = convert_mathml Node.math op.op_linenum txt
			in check_op op `Feature_mathml_inl elem

		| `AST_bold (comm, seq) ->
			let elem () = Some (Node.bold (convert_super_seq seq))
			in check_comm comm `Feature_bold elem

		| `AST_emph (comm, seq) ->
			let elem () = Some (Node.emph (convert_super_seq seq))
			in check_comm comm `Feature_emph elem

		| `AST_mono (comm, seq) ->
			let elem () = Some (Node.mono (convert_super_seq seq))
			in check_comm comm `Feature_mono elem

		| `AST_caps (comm, seq) ->
			let elem () = Some (Node.caps (convert_super_seq seq))
			in check_comm comm `Feature_caps elem

		| `AST_thru (comm, seq) ->
			let elem () = Some (Node.thru (convert_super_seq seq))
			in check_comm comm `Feature_thru elem

		| `AST_sup (comm, seq) ->
			let elem () = Some (Node.sup (convert_super_seq seq))
			in check_comm comm `Feature_sup elem

		| `AST_sub (comm, seq) ->
			let elem () = Some (Node.sub (convert_super_seq seq))
			in check_comm comm `Feature_sub elem

		| `AST_box (comm, seq) ->
			let elem () = Some (Node.box (convert_super_seq seq))
			in check_comm comm `Feature_box elem


	and convert_link_node : Document_ast.link_node_t -> (Node.link_node_t, _) Node.t option = function

		| `AST_link (comm, lnk, seq) ->
			let elem () = Some (Node.link lnk (convert_nonlink_seq seq))
			in check_comm comm `Feature_link elem

		| `AST_see (comm, label) ->
			let elem () =
				let target_checker = function
					| Order.Note_order _	-> `Valid_target
					| _			-> `Wrong_target Error.Target_note
				in add_reference target_checker comm label;
				Some (Node.see label)
			in check_comm comm `Feature_see elem

		| `AST_cite (comm, label) ->
			let elem () =
				let target_checker = function
					| Order.Bib_order _	-> `Valid_target
					| _			-> `Wrong_target Error.Target_bib
				in add_reference target_checker comm label;
				Some (Node.cite label)
			in check_comm comm `Feature_cite elem

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
			in check_comm comm `Feature_ref elem

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
			in check_comm comm `Feature_sref elem

		| `AST_mref (comm, label, seq) ->
			let elem () =
				let target_checker = function
					| Order.Block_order _	-> `Valid_target
					| _			-> `Wrong_target Error.Target_label
				in add_reference target_checker comm label;
				Some (Node.mref label (convert_nonlink_seq seq))
			in check_comm comm `Feature_mref elem


	and convert_textual_node : Document_ast.textual_node_t -> (Node.textual_node_t, _) Node.t option = function

		| `AST_plain txt ->
			let elem () = Some (Node.plain txt)
			in check_naked `Feature_plain elem

		| `AST_entity txt ->
			let elem () = Some (Node.entity txt)
			in check_naked `Feature_entity elem in


	(************************************************************************)
	(* Postprocessing functions for tabular environment.			*)
	(************************************************************************)

	let convert_tabular comm tab =
		let tcols = match comm.comm_secondary with
			| None		-> [| |]
			| Some thing	-> Array.map (get_column comm) (Array.of_list (String.explode thing)) in

		let num_columns = Array.length tcols in

		let convert_row (op, row) =
			(if List.length row <> num_columns
			then	let msg = Error.Wrong_column_number (comm.comm_linenum, List.length row, num_columns)
				in DynArray.add errors (op.op_linenum, msg));
			match row with
				| []		-> failwith "Parser has given us an empty tabular row"
				| hd::tl	-> Tabular.make_row (fplus convert_super_seq hd tl) in

		let convert_group (maybe_comm, rows) =
			let () = match maybe_comm with
				| Some comm	-> Permission.check errors comm Permission.forbidden_class
				| None		-> ()
			in match rows with
				| []		-> failwith "Parser has given us an empty tabular group"
				| hd::tl	-> fplus convert_row hd tl in

		let thead = match tab.thead with
			| Some grp	-> Some (convert_group grp)
			| None		-> None

		and tfoot = match tab.tfoot with
			| Some grp	-> Some (convert_group grp)
			| None		-> None

		in match tab.tbodies with
			| []		-> failwith "Parser has given us an empty tabular body"
			| hd::tl	-> Tabular.make tcols ?thead ?tfoot (fplus convert_group hd tl) in


	(************************************************************************)
	(* Postprocessing functions for document blocks.			*)
	(************************************************************************)

	let rec convert_super_frag subpaged frag =
		ExtList.List.filter_map (convert_super_block subpaged) frag


	and convert_nestable_frag subpaged frag =
		ExtList.List.filter_map (convert_nestable_block subpaged) frag


	and convert_super_block : bool -> Document_ast.super_block_t -> (Block.super_block_t, _) Block.t option = function subpaged -> function

		| `AST_top_block node ->
			(convert_top_block subpaged node :> (Block.super_block_t, _) Block.t option)

		| `AST_nestable_block node ->
			(convert_nestable_block subpaged node :> (Block.super_block_t, _) Block.t option)


	and convert_top_block : bool -> Document_ast.top_block_t -> (Block.top_block_t, _) Block.t option = function subpaged -> function

		| `AST_heading heading ->
			(convert_heading subpaged heading :> (Block.top_block_t, _) Block.t option)

		| `AST_appendix comm ->
			let elem () =
				appendixed := true;
				None
			in check_comm comm `Feature_appendix elem

		| `AST_rule comm ->
			let elem () = Some (Block.rule ())
			in check_comm comm `Feature_rule elem

		| `AST_setting (comm, key, value) ->
			let elem () =
				add_setting comm key value;
				None
			in check_comm comm `Feature_setting elem


	and convert_heading : bool -> Document_ast.heading_block_t -> (Block.top_block_t, _) Block.t option = function subpaged -> function

		| `AST_section (comm, seq) ->
			let elem () =
				let block =
					if !appendixed
					then
						let order = match comm.comm_order with
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
						let order = match comm.comm_order with
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
			in check_comm comm `Feature_section elem

		| `AST_subsection (comm, seq) ->
			let elem () =
				let block =
					if !appendixed
					then
						let order = match comm.comm_order with
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
						let order = match comm.comm_order with
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
			in check_comm comm `Feature_subsection elem

		| `AST_subsubsection (comm, seq) ->
			let elem () =
				let block =
					if !appendixed
					then
						let order = match comm.comm_order with
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
						let order = match comm.comm_order with
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
			in check_comm comm `Feature_subsubsection elem

		| `AST_toc comm ->
			let elem () =
				let order = `None_order in
				let label = make_label comm (Order.preset_sectional_order order)
				in Some (Block.toc label order)
			in check_comm comm `Feature_toc elem

		| `AST_bibliography comm ->
			let elem () =
				let order = `None_order in
				let label = make_label comm (Order.preset_sectional_order order) in
				let block = Block.bibliography label order in
				(if not subpaged then add_toc_entry block);
				Some block
			in check_comm comm `Feature_bibliography elem

		| `AST_notes comm ->
			let elem () =
				let order = `None_order in
				let label = make_label comm (Order.preset_sectional_order order) in
				let block = Block.notes label order in
				(if not subpaged then add_toc_entry block);
				Some block
			in check_comm comm `Feature_notes elem


	and convert_nestable_block : bool -> Document_ast.nestable_block_t -> (Block.nestable_block_t, _) Block.t option = function subpaged -> function

		| `AST_paragraph (op, seq) ->
			let elem () = Some (Block.paragraph (convert_super_seq seq))
			in check_op op `Feature_paragraph elem

		| `AST_itemize (comm, items) ->
			let elem () =
				let bullet = get_bullet comm comm.comm_extra
				in Some (Block.itemize bullet (convert_items subpaged items))
			in check_comm comm `Feature_itemize elem

		| `AST_enumerate (comm, items) ->
			let elem () =
				let numbering = get_numbering comm comm.comm_extra
				in Some (Block.enumerate numbering (convert_items subpaged items))
			in check_comm comm `Feature_enumerate elem

		| `AST_quote (comm, frag) ->
			let elem () =
				let alignment = get_alignment comm comm.comm_extra
				in Some (Block.quote alignment (convert_nestable_frag subpaged frag))
			in check_comm comm `Feature_quote elem

		| `AST_mathtex_blk (comm, txt) ->
			let elem () =
				let alignment = get_alignment comm comm.comm_extra
				in convert_mathtex (Block.math alignment) comm.comm_linenum txt
			in check_comm comm `Feature_mathtex_blk elem

		| `AST_mathml_blk (comm, txt) ->
			let elem () =
				let alignment = get_alignment comm comm.comm_extra
				in convert_mathml (Block.math alignment) comm.comm_linenum txt
			in check_comm comm `Feature_mathml_blk elem

		| `AST_code (comm, seq) ->
			let elem () =
				let alignment = get_alignment comm comm.comm_extra
				and syntax = comm.comm_secondary
				in Some (Block.code alignment syntax (convert_textual_seq seq))
			in check_comm comm `Feature_code elem

		| `AST_verbatim (comm, seq) ->
			let elem () =
				let alignment = get_alignment comm comm.comm_extra
				in Some (Block.verbatim alignment (convert_textual_seq seq))
			in check_comm comm `Feature_verbatim elem

		| `AST_tabular (comm, tab) ->
			let elem () =
				let alignment = get_alignment comm comm.comm_extra
				in Some (Block.tabular alignment (convert_tabular comm tab))
			in check_comm comm `Feature_tabular elem

		| `AST_image (comm, alias) ->
			let elem () =
				let alignment = get_alignment comm comm.comm_extra
				in Some (Block.image alignment alias)
			in check_comm comm `Feature_image elem

		| `AST_subpage (comm, subpage) ->
			let elem () =
				let alignment = get_alignment comm comm.comm_extra
				in Some (Block.subpage alignment (convert_super_frag true subpage))
			in check_comm comm `Feature_subpage elem

		| `AST_equation (comm, cap, eq) ->
			let elem () =
				let maybe_floater = convert_floater comm equation_counter Order.equation_order cap
				and maybe_equation = convert_equation eq
				in match (maybe_floater, maybe_equation) with
					| (Some floater, Some equation)		-> Some (Block.equation floater equation)
					| _					-> None
			in check_comm comm `Feature_equation elem

		| `AST_algorithm (comm, cap, alg) ->
			let elem () =
				let maybe_floater = convert_floater comm algorithm_counter Order.algorithm_order cap
				and maybe_algorithm = convert_algorithm alg
				in match (maybe_floater, maybe_algorithm) with
					| (Some floater, Some algorithm)	-> Some (Block.algorithm floater algorithm)
					| _					-> None
			in check_comm comm `Feature_algorithm elem

		| `AST_table (comm, cap, tab) ->
			let elem () =
				let maybe_floater = convert_floater comm table_counter Order.table_order cap
				and maybe_table = convert_table tab
				in match (maybe_floater, maybe_table) with
					| (Some floater, Some table)		-> Some (Block.table floater table)
					| _					-> None
			in check_comm comm `Feature_table elem

		| `AST_figure (comm, cap, fig) ->
			let elem () =
				let maybe_floater = convert_floater comm figure_counter Order.figure_order cap
				and maybe_figure = convert_figure fig
				in match (maybe_floater, maybe_figure) with
					| (Some floater, Some figure)		-> Some (Block.figure floater figure)
					| _					-> None
			in check_comm comm `Feature_figure elem

		| `AST_bib (comm, title, author, resource) ->
			let elem () =
				let order = make_ghost_order comm bib_counter in
				let label = make_label comm (Order.bib_order order)
				and title = convert_bib_title title
				and author = convert_bib_author author
				and resource = convert_bib_resource resource
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
			in check_comm comm `Feature_bib elem

		| `AST_note (comm, seq) ->
			let elem () =
				let order = make_ghost_order comm note_counter in
				let label = make_label comm (Order.note_order order) in
				let note =
					{
					Note.label = label;
					Note.order = order;
					Note.content = convert_super_seq seq;
					}
				in DynArray.add notes note;
				None
			in check_comm comm `Feature_note elem


	and convert_item subpaged : Document_ast.item_block_t -> (Block.nestable_block_t, _) Block.t list = function

		| `AST_item (comm, frag) ->
			convert_nestable_frag subpaged frag


	and convert_items subpaged = function
		| []		-> failwith "Parser has given us an empty list!"
		| hd::tl	-> fplus (convert_item subpaged) hd tl


	and convert_equation : Document_ast.equation_block_t -> Block.equation_block_t option = function

		| `AST_mathtex_blk (comm, txt) ->
			let elem () =
				let alignment = get_alignment comm comm.comm_extra
				in convert_mathtex (Block.math alignment) comm.comm_linenum txt
			in check_comm comm `Feature_mathtex_blk elem

		| `AST_mathml_blk (comm, txt) ->
			let elem () =
				let alignment = get_alignment comm comm.comm_extra
				in convert_mathml (Block.math alignment) comm.comm_linenum txt
			in check_comm comm `Feature_mathml_blk elem


	and convert_algorithm : Document_ast.algorithm_block_t -> Block.algorithm_block_t option = function

		| `AST_code (comm, seq) ->
			let elem () =
				let alignment = get_alignment comm comm.comm_extra
				and syntax = comm.comm_secondary
				in Some (Block.code alignment syntax (convert_textual_seq seq))
			in check_comm comm `Feature_code elem


	and convert_table : Document_ast.table_block_t -> Block.table_block_t option = function

		| `AST_tabular (comm, tab) ->
			let elem () =
				let alignment = get_alignment comm comm.comm_extra
				in Some (Block.tabular alignment (convert_tabular comm tab))
			in check_comm comm `Feature_tabular elem


	and convert_figure : Document_ast.figure_block_t -> Block.figure_block_t option = function

		| `AST_verbatim (comm, seq) ->
			let elem () =
				let alignment = get_alignment comm comm.comm_extra
				in Some (Block.verbatim alignment (convert_textual_seq seq))
			in check_comm comm `Feature_verbatim elem

		| `AST_image (comm, alias) ->
			let elem () =
				let alignment = get_alignment comm comm.comm_extra
				in Some (Block.image alignment alias)
			in check_comm comm `Feature_image elem

		| `AST_subpage (comm, subpage) ->
			let elem () =
				let alignment = get_alignment comm comm.comm_extra
				in Some (Block.subpage alignment (convert_super_frag true subpage))
			in check_comm comm `Feature_subpage elem


	and convert_caption : Document_ast.caption_block_t -> (Node.super_node_t, _) Node.t list option = function

		| `AST_caption (comm, seq) ->
			let elem () = Some (convert_super_seq seq)
			in check_comm comm `Feature_caption elem


	and convert_bib_title : Document_ast.bib_title_block_t -> (Node.super_node_t, _) Node.t list option = function

		| `AST_bib_title (comm, seq) ->
			let elem () = Some (convert_super_seq seq)
			in check_comm comm `Feature_bib_title elem


	and convert_bib_author : Document_ast.bib_author_block_t -> (Node.super_node_t, _) Node.t list option = function

		| `AST_bib_author (comm, seq) ->
			let elem () = Some (convert_super_seq seq)
			in check_comm comm `Feature_bib_author elem


	and convert_bib_resource : Document_ast.bib_resource_block_t -> (Node.super_node_t, _) Node.t list option = function

		| `AST_bib_resource (comm, seq) ->
			let elem () = Some (convert_super_seq seq)
			in check_comm comm `Feature_bib_resource elem


	and convert_floater comm counter order_func cap =
		let order = make_floater_order comm counter in
		let label = make_label comm (order_func order) in
		let maybe_caption = convert_caption cap
		in match (label, order, maybe_caption) with
			| (label, order, Some caption)	-> Some (label, order, caption)
			| _				-> None


	(************************************************************************)
	(* Wrap-up.								*)
	(************************************************************************)

	and filter_references () =
		let filter_reference (target_checker, comm, label) =
			try
				let target = Hashtbl.find labels (`User_label label) in
				match target_checker target with
				| `Valid_target ->
					()
				| `Empty_target ->
					let msg = Error.Empty_target (comm.comm_tag, label)
					in DynArray.add errors (comm.comm_linenum, msg)
				| `Wrong_target expected ->
					let suggestion = match target with
						| Order.Block_order _	-> Error.Target_label
						| Order.Bib_order _	-> Error.Target_bib
						| Order.Note_order _	-> Error.Target_note in
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
	filter_references ();
	let res_bibs = DynArray.to_list bibs
	and res_notes = DynArray.to_list notes
	and res_toc = DynArray.to_list toc
	and res_errors = DynArray.to_list errors
	in (contents, res_bibs, res_notes, res_toc, labels, !settings, res_errors)


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
(**	{2 Public functions}							*)
(********************************************************************************)

let process_manuscript source document_ast =
	let (contents, bibs, notes, toc, labels, settings, errors) = process_document document_ast in
	if List.length errors = 0
	then
		Ambivalent.make_valid_manuscript contents bibs notes toc labels settings
	else
		let sorted_errors = sort_errors (collate_errors source errors)
		in Ambivalent.make_invalid_manuscript sorted_errors


let process_composition source document_ast =
	let (contents, _, _, _, _, _, errors) = process_document document_ast in
	if List.length errors = 0
	then
		let composition = Document_convert.convert_to_composition contents
		in Ambivalent.make_valid_composition composition
	else
		let sorted_errors = sort_errors (collate_errors source errors)
		in Ambivalent.make_invalid_composition sorted_errors

