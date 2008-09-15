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
	let check errors params (perm_label, perm_order, perm_extra, perm_secondary) =

		(match reason_why_invalid perm_label params.comm_label with
			| None ->
				()
			| Some reason ->
				let msg = Error.Bad_label_parameter (params.comm_tag, reason) in
				DynArray.add errors (params.comm_linenum, msg));

		(match reason_why_invalid perm_order params.comm_order with
			| None ->
				()
			| Some reason ->
				let msg = Error.Bad_order_parameter (params.comm_tag, reason) in
				DynArray.add errors (params.comm_linenum, msg));

		(match reason_why_invalid perm_extra params.comm_extra with
			| None ->
				()
			| Some reason ->
				let msg = Error.Bad_extra_parameter (params.comm_tag, reason) in
				DynArray.add errors (params.comm_linenum, msg));

		(match reason_why_invalid perm_secondary params.comm_secondary with
			| None ->
				()
			| Some reason ->
				let msg = Error.Bad_secondary_parameter (params.comm_tag, reason) in
				DynArray.add errors (params.comm_linenum, msg))
end


(********************************************************************************)
(**	{2 Private functions}							*)
(********************************************************************************)

(**	Processes an AST as provided by the parser, producing the corresponding
	document.  In addition, a list of labels, bibliography entries, notes,
	and possible errors is also returned.
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


	(*	This subfunction returns the type of bullet associated with
		an itemize environment.  If no extra parameter is given, the
		bullet type is assumed to be the default.
	*)
	let get_bullet params = function
		| None ->
			Bullet.Default
		| Some thing ->
			try
				Bullet.of_string thing
			with
				Bullet.Unknown_bullet_type x ->
					let msg = Error.Unknown_bullet_type (params.comm_tag, x) in
					DynArray.add errors (params.comm_linenum, msg);
					Bullet.Default


	(*	This subfunction returns the type of numbering associated with
		an enumerate environment.  If no extra parameter is given, the
		numbering type is assumed to be the default.
	*)
	and get_numbering params = function
		| None ->
			Numbering.Default
		| Some thing ->
			try
				Numbering.of_string thing
			with
				Numbering.Unknown_numbering_type x ->
					let msg = Error.Unknown_numbering_type (params.comm_tag, x) in
					DynArray.add errors (params.comm_linenum, msg);
					Numbering.Default


	(**	This subfunction returns the type of alignment associated with
		a floater or quote environment.  If no extra parameter is given,
		the alignment type is assumed to be [Center].
	*)
	and get_alignment params = function
		| None	->
			Alignment.Center
		| Some thing ->
			try
				Alignment.of_string thing
			with
				Alignment.Unknown_alignment_type x ->
					let msg = Error.Unknown_alignment_type (params.comm_tag, x) in
					DynArray.add errors (params.comm_linenum, msg);
					Alignment.Center


	(**	This function returns the column alignment and weight associated
		with a column specifier.
	*)
	and get_column params spec =
		try
			Tabular.column_of_specifier spec
		with
			Tabular.Invalid_column_specifier spec ->
				let msg = Error.Invalid_column_specifier (params.comm_tag, spec)
				in DynArray.add errors (params.comm_linenum, msg);
				(Tabular.Center, Tabular.Normal) in


	(*	This function returns a boolean indicating whether the floater
		has a caption block.
	*)
	let check_captioned blocks =
		List.exists (function `Caption _ -> true | _ -> false) blocks


	(*	Adds a new floater block.  Note that the contents of the block
		are only computed if the block is valid, ie, no previous block
		of the same type exists.  This is achieved via lazy types.
	*)
	and add_block params block positive =
		match !block with
		| None 		-> block := Some (Lazy.force positive)
		| Some thing	-> DynArray.add errors (params.comm_linenum, Error.Duplicate_block params.comm_tag)


	(*	Determines whether a mandatory block exists or not.  If the block
		does not exist, an error is reported and the [default] value is
		assigned to the block.
	*)
	and check_block_existence params block default tag =
		match !block with
		| None ->
			let msg = Error.Missing_block (params.comm_tag, tag) in
			DynArray.add errors (params.comm_linenum, msg);
			default
		| Some thing ->
			thing


	(*	Reports the existence of an invalid block.  This is used in figure
		environments because the parser is not designed to determine by
		itself whether a block is valid for a particular kind of figure.
	*)
	and report_invalid_block params main_tag =
		let msg = Error.Invalid_block (main_tag, params.comm_tag) in
		DynArray.add errors (params.comm_linenum, msg)


	(*	This subfunction creates a new label.  It checks whether the user explicitly
		provide a label (in which case we use the `User_label variant), or if no
		label was defined (in which case we automatically assign a label using the
		`Auto_label variant).
	*)
	and make_label params order =
		match params.comm_label with
		| Some thing ->
			let new_label = `User_label thing in
			(if Hashtbl.mem labels new_label
			then DynArray.add errors (params.comm_linenum, (Error.Duplicate_label (params.comm_tag, thing)))
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
	and make_floater_order captioned params counter =
		match (captioned, params.comm_order) with
			| (true, None) ->
				incr counter;
				`Auto_order (Order.Numeric [!counter])
			| (true, Some thing) ->
				`User_order thing
			| _ ->
				`None_order


	(*	This subfunction creates a new ghost order.  Since ghost elements do not
		accept user provided ordering, the order is always computed automatically.
	*)
	and make_ghost_order params counter =
		incr counter;
		`Auto_order (Order.Numeric [!counter])


	(*	Adds a new reference to the dictionary.
	*)
	and add_reference target_checker params label =
		DynArray.add references (target_checker, params, label)


	(*	Adds a new TOC entry.
	*)
	and add_toc_entry = function
		| `Heading blk	-> DynArray.add toc blk
		| _		-> failwith "Oops: attempted to add non-heading entry into TOC"


	(*	Adds a new setting.
	*)
	and add_setting params key value =
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
				settings := Settings.set_default_bullet !settings (get_bullet params (Some value))
			| "default_numbering" ->
				settings := Settings.set_default_numbering !settings (get_numbering params (Some value))
			| _ ->
				DynArray.add errors (params.comm_linenum, Error.Unknown_setting key) in

	(*	Converts a block with maths.
	*)
	let rec convert_math_block params txt =
		match params.comm_secondary with
			| Some "tex" ->
				(try
					let math = Math.from_mathtex txt
					in Some (Block.math math)
				with Math.Invalid_mathtex ->
					let msg = Error.Invalid_mathtex txt in
					DynArray.add errors (params.comm_linenum, msg);
					None)
			| Some "mathml" ->
				(try
					let math  = Math.from_mathml txt
					in Some (Block.math math)
				with Math.Invalid_mathml ->
					let msg = Error.Invalid_mathml txt in
					DynArray.add errors (params.comm_linenum, msg);
					None)
			| Some other ->
				let msg = Error.Unknown_math_type (params.comm_tag, other) in
				DynArray.add errors (params.comm_linenum, msg);
				None
			| None ->
				None

	(*	Converts a tabular environment.
	*)
	and convert_tabular params tab =
		Permission.check errors params Permission.tabular_class;

		let tcols = match params.comm_secondary with
			| None		-> [| |]
			| Some thing	-> Array.map (get_column params) (Array.of_list (String.explode thing)) in

		let num_columns = Array.length tcols in

		let convert_row (op, row) =
			(if List.length row <> num_columns
			then	let msg = Error.Wrong_column_number (params.comm_linenum, List.length row, num_columns)
				in DynArray.add errors (op.op_linenum, msg));
			match row with
				| []		-> failwith "Parser has given us an empty tabular row"
				| hd::tl	-> Tabular.make_row (fplus convert_super_seq hd tl) in

		let convert_group (maybe_params, rows) =
			(match maybe_params with
				| Some params	-> Permission.check errors params Permission.forbidden_class
				| None		-> ());
			match rows with
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
			| hd::tl	-> Tabular.make tcols ?thead ?tfoot (fplus convert_group hd tl)


	(*	Converts the blocks inside an algorithm environment.
	*)
	and convert_algorithm_blocks params blocks =
		let caption = ref None
		and verbatim = ref None in
		let process_block = function
			| `Caption (params, seq) ->
				add_block params caption (lazy (convert_super_seq seq))
			| `Verbatim (params, seq) ->
				add_block params verbatim (lazy (convert_textual_seq seq))
		in List.iter process_block blocks;
		let res_verbatim = check_block_existence params verbatim [] "verbatim"
		in (!caption, res_verbatim)


	(*	Converts the blocks inside an equation environment.
	*)
	and convert_equation_blocks params blocks =
		let caption = ref None
		and math = ref None in
		let process_block = function
			| `Caption (params, seq) ->
				add_block params caption (lazy (convert_super_seq seq))
			| `Math (params, text) ->
				add_block params math (lazy text)
		in List.iter process_block blocks;
		let res_math = check_block_existence params math "" "math"
		in (!caption, res_math)


	(*	Converts the blocks inside a figure environment.
	*)
	and convert_figure subpaged alignment label order params blocks =

		let convert_figure_load_blocks main_tag blocks =
			let caption = ref None
			and load = ref None in
			let process_block = function
				| `Caption (params, seq) ->
					add_block params caption (lazy (convert_super_seq seq))
				| `Load (params, txt) ->
					add_block params load (lazy txt)
				| `Verbatim (params, seq) ->
					report_invalid_block params main_tag
				| `Subpage (params, frag) ->
					report_invalid_block params main_tag
			in List.iter process_block blocks;
			let res_load = check_block_existence params load "" "load"
			in (!caption, res_load)

		and convert_figure_ascii_blocks main_tag blocks =
			let caption = ref None
			and verbatim = ref None in
			let process_block = function
				| `Caption (params, seq) ->
					add_block params caption (lazy (convert_super_seq seq))
				| `Load (params, text) ->
					report_invalid_block params main_tag
				| `Verbatim (params, seq) ->
					add_block params verbatim (lazy (convert_textual_seq seq))
				| `Subpage (params, frag) ->
					report_invalid_block params main_tag
			in List.iter process_block blocks;
			let res_verbatim = check_block_existence params verbatim [] "verbatim"
			in (!caption, res_verbatim)

		and convert_figure_subpage_blocks main_tag blocks =
			let caption = ref None
			and subpage = ref None in
			let process_block = function
				| `Caption (params, seq) ->
					add_block params caption (lazy (convert_super_seq seq))
				| `Load (params, text) ->
					report_invalid_block params main_tag
				| `Verbatim (params, seq) ->
					report_invalid_block params main_tag
				| `Subpage (params, frag) ->
					add_block params subpage (lazy (convert_super_frag true frag))
			in List.iter process_block blocks;
			let res_subpage = check_block_existence params subpage [] "subpage"
			in (!caption, res_subpage)

		in match params.comm_secondary with
			| Some "bitmap" ->
				let (caption, load) = convert_figure_load_blocks "bitmap" blocks in
				Some (Block.figure_bitmap alignment label order caption load)
			| Some "vector" ->
				let (caption, load) = convert_figure_load_blocks "vector" blocks in
				Some (Block.figure_vector alignment label order caption load)
			| Some "ascii" ->
				let (caption, verbatim) = convert_figure_ascii_blocks "ascii" blocks in
				Some (Block.figure_ascii alignment label order caption verbatim)
			| Some "subpage" ->
				let (caption, subpage) = convert_figure_subpage_blocks "subpage" blocks in
				Some (Block.figure_subpage alignment label order caption subpage)
			| Some other ->
				let msg = Error.Unknown_figure_type (params.comm_tag, other) in
				DynArray.add errors (params.comm_linenum, msg);
				None
			| None ->
				None


	(*	Converts the blocks inside a table environment.
	*)
	and convert_table_blocks main_params blocks =

		let caption = ref None
		and tabular = ref None in

		let process_block = function
			| `Caption (params, seq) ->
				add_block main_params caption (lazy (convert_super_seq seq))
			| `Tabular (params, tab) ->
				add_block main_params tabular (lazy (convert_tabular params tab))
		in List.iter process_block blocks;
		let default = Tabular.dummy () in
		let res_tabular = check_block_existence main_params tabular default "tabular"
		in (!caption, res_tabular)


	(*	Converts the blocks inside a bib environment.
	*)
	and convert_bib_blocks params blocks =
		let title = ref None
		and author = ref None
		and resource = ref None in
		let process_block = function
			| `Title (params, seq) ->
				add_block params title (lazy (convert_super_seq seq))
			| `Author (params, seq) ->
				add_block params author (lazy (convert_super_seq seq))
			| `Resource (params, seq) ->
				add_block params resource (lazy (convert_super_seq seq))
		in List.iter process_block blocks;
		let res_title = check_block_existence params title [] "title"
		and res_author = check_block_existence params author [] "author"
		and res_resource = check_block_existence params resource [] "resource"
		in (res_title, res_author, res_resource)


	(*	The following functions take care of converting the actual nodes,
		blocks, sequences, and fragments that constitute a document.
	*)

	and convert_textual_node = function
		| Plain txt ->
			Some (Node.plain txt)
		| Entity txt ->
			Some (Node.entity txt)

	and convert_nonlink_node = function
		| Textual node ->
			((convert_textual_node node) :> (Node.nonlink_node_t, _) Node.t option)
		| Mathtex (op, txt) ->
			(try
				Some (Node.math (Math.from_mathtex txt))
			with
				Math.Invalid_mathtex ->
					DynArray.add errors (op.op_linenum, Error.Invalid_mathtex txt);
					None)
		| Mathml (op, txt) ->
			(try
				Some (Node.math (Math.from_mathml txt))
			with
				Math.Invalid_mathml ->
					DynArray.add errors (op.op_linenum, Error.Invalid_mathml txt);
					None)
		| Bold (params, seq) ->
			Permission.check errors params Permission.forbidden_class;
			Some (Node.bold (convert_super_seq seq))
		| Emph (params, seq) ->
			Permission.check errors params Permission.forbidden_class;
			Some (Node.emph (convert_super_seq seq))
		| Mono (params, seq) ->
			Permission.check errors params Permission.forbidden_class;
			Some (Node.mono (convert_super_seq seq))
		| Caps (params, seq) ->
			Permission.check errors params Permission.forbidden_class;
			Some (Node.caps (convert_super_seq seq))
		| Thru (params, seq) ->
			Permission.check errors params Permission.forbidden_class;
			Some (Node.thru (convert_super_seq seq))
		| Sup (params, seq) ->
			Permission.check errors params Permission.forbidden_class;
			Some (Node.sup (convert_super_seq seq))
		| Sub (params, seq) ->
			Permission.check errors params Permission.forbidden_class;
			Some (Node.sub (convert_super_seq seq))
		| Box (params, seq) ->
			Permission.check errors params Permission.forbidden_class;
			Some (Node.box (convert_super_seq seq))

	and convert_link_node = function
		| Link (params, lnk, seq) ->
			Permission.check errors params Permission.forbidden_class;
			Some (Node.link lnk (convert_nonlink_seq seq))
		| See (params, label) ->
			Permission.check errors params Permission.forbidden_class;
			let target_checker = function
				| Order.Note_order _ ->
					`Valid_target
				| _ ->
					`Wrong_target Error.Target_note
			in add_reference target_checker params label;
			Some (Node.see label)
		| Cite (params, label) ->
			Permission.check errors params Permission.forbidden_class;
			let target_checker = function
				| Order.Bib_order _ ->
					`Valid_target
				| _ ->
					`Wrong_target Error.Target_bib
			in add_reference target_checker params label;
			Some (Node.cite label)
		| Ref (params, label) ->
			Permission.check errors params Permission.forbidden_class;
			let target_checker = function
				| Order.Block_order (Order.Body_sectional_order `None_order)
				| Order.Block_order (Order.Appendix_sectional_order `None_order)
				| Order.Block_order (Order.Preset_sectional_order `None_order)
				| Order.Block_order (Order.Floater_order (_, `None_order)) ->
					`Empty_target
				| Order.Block_order _ ->
					`Valid_target
				| _ ->
					`Wrong_target Error.Target_label
			in add_reference target_checker params label;
			Some (Node.ref label)
		| Sref (params, label) ->
			Permission.check errors params Permission.forbidden_class;
			let target_checker = function
				| Order.Block_order (Order.Body_sectional_order `None_order)
				| Order.Block_order (Order.Appendix_sectional_order `None_order)
				| Order.Block_order (Order.Preset_sectional_order `None_order)
				| Order.Block_order (Order.Floater_order (_, `None_order)) ->
					`Empty_target
				| Order.Block_order _ ->
					`Valid_target
				| _ ->
					`Wrong_target Error.Target_label
			in add_reference target_checker params label;
			Some (Node.sref label)
		| Mref (params, label, seq) ->
			Permission.check errors params Permission.forbidden_class;
			let target_checker = function
				| Order.Block_order _ ->
					`Valid_target
				| _ ->
					`Wrong_target Error.Target_label
			in add_reference target_checker params label;
			Some (Node.mref label (convert_nonlink_seq seq))

	and convert_super_node = function
		| Nonlink_node node ->
			(convert_nonlink_node node :> (Node.super_node_t, _) Node.t option)
		| Link_node node ->
			convert_link_node node

	and convert_textual_seq seq =
		ExtList.List.filter_map convert_textual_node seq

	and convert_super_seq seq =
		ExtList.List.filter_map convert_super_node seq

	and convert_nonlink_seq seq =
		ExtList.List.filter_map convert_nonlink_node seq

	and convert_heading subpaged = function

		| Section (params, seq) ->
			Permission.check errors params (Permission.user_sectional_class subpaged);
			let block =
				if !appendixed
				then
					let order = match params.comm_order with
						| None ->
							(appendix_counter := !appendix_counter + 1;
							subappendix_counter := 0;
							subsubappendix_counter := 0;
							`Auto_order (Order.Appendic [!appendix_counter]))
						| Some "" ->
							`None_order
						| Some other ->
							`User_order other in
					let label = make_label params (Order.appendix_sectional_order order)
					in Block.appendix label order (convert_super_seq seq)
				else
					let order = match params.comm_order with
						| None ->
							(section_counter := !section_counter + 1;
							subsection_counter := 0;
							subsubsection_counter := 0;
							`Auto_order (Order.Numeric [!section_counter]))
						| Some "" ->
							`None_order
						| Some other ->
							`User_order other in
					let label = make_label params (Order.body_sectional_order order)
					in Block.section label order (convert_super_seq seq)
			in (if not subpaged then add_toc_entry block);
			Some (block)

		| Subsection (params, seq) ->
			Permission.check errors params (Permission.user_sectional_class subpaged);
			let block =
				if !appendixed
				then
					let order = match params.comm_order with
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
					let label = make_label params (Order.appendix_sectional_order order)
					in Block.subappendix label order (convert_super_seq seq)
				else
					let order = match params.comm_order with
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
					let label = make_label params (Order.body_sectional_order order)
					in Block.subsection label order (convert_super_seq seq)
			in (if not subpaged then add_toc_entry block);
			Some (block)

		| Subsubsection (params, seq) ->
			Permission.check errors params (Permission.user_sectional_class subpaged);
			let block =
				if !appendixed
				then
					let order = match params.comm_order with
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
					let label = make_label params (Order.appendix_sectional_order order)
					in Block.subsubappendix label order (convert_super_seq seq)
				else
					let order = match params.comm_order with
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
					let label = make_label params (Order.body_sectional_order order)
					in Block.subsubsection label order (convert_super_seq seq)
			in (if not subpaged then add_toc_entry block);
			Some (block)

		| Toc params ->
			Permission.check errors params Permission.preset_sectional_class;
			let order = `None_order in
			let label = make_label params (Order.preset_sectional_order order) in
			let block = Block.toc label order in
			Some (block)

		| Bibliography params ->
			Permission.check errors params Permission.preset_sectional_class;
			let order = `None_order in
			let label = make_label params (Order.preset_sectional_order order) in
			let block = Block.bibliography label order in
			(if not subpaged then add_toc_entry block);
			Some (block)

		| Notes params ->
			Permission.check errors params Permission.preset_sectional_class;
			let order = `None_order in
			let label = make_label params (Order.preset_sectional_order order) in
			let block = Block.notes label order in
			(if not subpaged then add_toc_entry block);
			Some (block)


	and convert_top_block subpaged = function

		| Heading heading ->
			convert_heading subpaged heading

		| Appendix params ->
			Permission.check errors params Permission.forbidden_class;
			appendixed := true;
			None

		| Rule params ->
			Permission.check errors params Permission.forbidden_class;
			Some (Block.rule ())

		| Setting (params, key, value) ->
			Permission.check errors params Permission.forbidden_class;
			add_setting params key value;
			None


	and convert_nestable_block subpaged = function

		| Paragraph (op, seq) ->
			Some (Block.paragraph (convert_super_seq seq))

		| Math (params, txt) ->
			Permission.check errors params Permission.math_class;
			convert_math_block params txt

		| Tabular (params, tab) ->
			Some (Block.tabular (convert_tabular params tab))

		| Preformat (params, seq) ->
			Permission.check errors params Permission.forbidden_class;
			Some (Block.preformat (convert_textual_seq seq))

		| Itemize (params, items) ->
			Permission.check errors params Permission.listing_class;
			let bullet = get_bullet params params.comm_extra in
			let constructor = Block.itemize bullet
			in convert_items subpaged constructor items

		| Enumerate (params, items) ->
			Permission.check errors params Permission.listing_class;
			let numbering = get_numbering params params.comm_extra in
			let constructor = Block.enumerate numbering 
			in convert_items subpaged constructor items

		| Quote (params, frag) ->
			Permission.check errors params Permission.quote_class;
			let alignment = get_alignment params params.comm_extra
			in Some (Block.quote alignment (convert_nestable_frag subpaged frag))

		| Algorithm (params, blocks) ->
			let captioned = check_captioned blocks
			in Permission.check errors params (Permission.algorithm_class subpaged captioned);
			let alignment = get_alignment params params.comm_extra in
			let order = make_floater_order captioned params algorithm_counter in
			let label = make_label params (Order.algorithm_order order) in
			let (caption, text) = convert_algorithm_blocks params blocks
			in Some (Block.algorithm alignment label order caption text None)

		| Equation (params, blocks) ->
			let captioned = check_captioned blocks
			in Permission.check errors params (Permission.equation_class subpaged captioned);
			let alignment = get_alignment params params.comm_extra in
			let order = make_floater_order captioned params equation_counter in
			let label = make_label params (Order.equation_order order) in
			let (caption, text) = convert_equation_blocks params blocks
			in Some (Block.equation alignment label order caption text)

		| Figure (params, blocks) ->
			let captioned = check_captioned blocks
			in Permission.check errors params (Permission.figure_class subpaged captioned);
			let alignment = get_alignment params params.comm_extra in
			let order = make_floater_order captioned params figure_counter in
			let label = make_label params (Order.figure_order order)
			in convert_figure subpaged alignment label order params blocks

		| Table (params, blocks) ->
			let captioned = check_captioned blocks
			in Permission.check errors params (Permission.table_class subpaged captioned);
			let alignment = get_alignment params params.comm_extra in
			let order = make_floater_order captioned params table_counter in
			let label = make_label params (Order.table_order order) in
			let (caption, table_data) = convert_table_blocks params blocks
			in Some (Block.table alignment label order caption table_data)

		| Bib (params, blocks) ->
			Permission.check errors params Permission.ghost_class;
			let order = make_ghost_order params bib_counter in
			let label = make_label params (Order.bib_order order) in
			let (title, author, resource) = convert_bib_blocks params blocks in
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

		| Note (params, seq) ->
			Permission.check errors params Permission.ghost_class;
			let order = make_ghost_order params note_counter in
			let label = make_label params (Order.note_order order) in
			let note =
				{
				Note.label = label;
				Note.order = order;
				Note.content = convert_super_seq seq;
				}
			in DynArray.add notes note;
			None

	and convert_item subpaged = function
		| Item (params, frag) ->
			Permission.check errors params Permission.forbidden_class;
			convert_nestable_frag subpaged frag

	and convert_items subpaged constructor = function
		| [] ->
			failwith "Parser has given us an empty list!"
		| hd::tl ->
			let head_item = convert_item subpaged hd
			and tail_items = List.map (convert_item subpaged) tl
			in Some (constructor (head_item, tail_items))

	and convert_super_block subpaged = function
		| Top_block blk ->
			convert_top_block subpaged blk
		| Nestable_block blk ->
			(convert_nestable_block subpaged blk :> (Block.super_block_t, _) Block.t option)

	and convert_nestable_frag subpaged frag =
		ExtList.List.filter_map (convert_nestable_block subpaged) frag

	and convert_super_frag subpaged frag =
		ExtList.List.filter_map (convert_super_block subpaged) frag

	and filter_references () =
		let filter_reference (target_checker, params, label) =
			try
				let target = Hashtbl.find labels (`User_label label) in
				match target_checker target with
				| `Valid_target ->
					()
				| `Empty_target ->
					let msg = Error.Empty_target (params.comm_tag, label)
					in DynArray.add errors (params.comm_linenum, msg)
				| `Wrong_target expected ->
					let suggestion = match target with
						| Order.Block_order _	-> Error.Target_label
						| Order.Bib_order _	-> Error.Target_bib
						| Order.Note_order _	-> Error.Target_note in
					let msg = Error.Wrong_target (params.comm_tag, expected, suggestion, label)
					in DynArray.add errors (params.comm_linenum, msg)
			with
				Not_found ->
					let msg = Error.Absent_target (params.comm_tag, label) in
					DynArray.add errors (params.comm_linenum, msg)
		in
		DynArray.iter filter_reference references in

	let contents = convert_super_frag false document_ast in
	filter_references ();
	let res_bibs = DynArray.to_list bibs
	and res_notes = DynArray.to_list notes
	and res_toc = DynArray.to_list toc
	and res_errors = DynArray.to_list errors
	in
	(contents, res_bibs, res_notes, res_toc, labels, !settings, res_errors)


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
	let (contents, bibs, notes, toc, labels, settings, process_errors) = process_document document_ast in
	if List.length process_errors = 0
	then
		Ambivalent.make_valid_manuscript contents bibs notes toc labels settings
	else
		let errors = sort_errors (collate_errors source process_errors)
		in Ambivalent.make_invalid_manuscript errors


let process_composition source document_ast =
	let (filtered_ast, filter_errors) = Document_filter.filter_to_composition document_ast in
	let (contents, _, _, _, _, _, process_errors) = process_document filtered_ast in
	let all_errors = List.append filter_errors process_errors in
	if List.length all_errors = 0
	then
		let composition = Document_convert.convert_to_composition contents
		in Ambivalent.make_valid_composition composition
	else
		let errors = sort_errors (collate_errors source all_errors)
		in Ambivalent.make_invalid_composition errors

