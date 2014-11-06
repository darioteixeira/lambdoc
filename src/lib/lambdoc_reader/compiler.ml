(********************************************************************************)
(*	Compiler.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Compilation of a document Ast.  These functions convert
	a document AST into a proper, final, ambivalent document.
*)

open Lambdoc_core
open Basic
open Ast
open Readconv
open Style
open Idiosyncrasies

module String = BatString
module List = BatList


(********************************************************************************)
(**	{1 Exceptions}								*)
(********************************************************************************)

exception Mismatched_custom of Custom.kind_t * Custom.kind_t


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type customdef_t =
	| Anonymous
	| Unnumbered of Inline.seq_t
	| Numbered of Inline.seq_t * Order_input.ordinal_counter_t ref


(********************************************************************************)
(**	{1 Auxiliary functions}							*)
(********************************************************************************)

let flatten_map f xs = List.flatten (List.map f xs)

let maybe f = function
	| Some x -> Some (f x)
	| None   -> None

let perhaps f = function
	| [x] -> [f x]
	| _   -> []


(********************************************************************************)
(**	{1 Functors}								*)
(********************************************************************************)

module Make (Ext: Extension.S) =
struct
	open Ext

	let (>>=) = Monad.bind


(********************************************************************************)
(**	{2 Generic document compilation}					*)
(********************************************************************************)

(**	Compiles an AST as provided by the parser, producing the corresponding
	document.  In addition, a label dictionary, bibliography entries, notes,
	and possible errors are also returned.
*)
let compile_document ?rconfig ~expand_entities ~idiosyncrasies ast =

	(************************************************************************)
	(* Declaration of some constant values used in the function.		*)
	(************************************************************************)

	(**	Is the usage of macros authorised for this document?  If not,
		we can save time by skipping the coalescing of plain elements.
		We determine whether macros are allowed or not by checking the
		idiosyncrasies of this particular document.
	*)
	let macros_authorised = Permission.check_feature `Feature_macrodef idiosyncrasies in


	(************************************************************************)
	(* Declaration of the local modules used in the function.		*)
	(************************************************************************)

	let module HrefSet = Set.Make (Href) in


	(************************************************************************)
	(* Declaration of the mutable values used in the function.		*)
	(************************************************************************)

	let pointers = BatDynArray.create ()
	and bibs = BatDynArray.create ()
	and notes = BatDynArray.create ()
	and toc = BatDynArray.create ()
	and linkrefs = BatDynArray.create ()
	and imagerefs = BatDynArray.create ()
	and extinlrefs = BatDynArray.create ()
	and extblkrefs = BatDynArray.create ()
	and linkset = ref HrefSet.empty
	and imageset = ref HrefSet.empty
        and labels = Hashtbl.create 10
	and customisations = Hashtbl.create 10
	and macros = Hashtbl.create 10
	and errors = BatDynArray.create ()
	and part_counter = Order_input.ordinal_counter ()
	and section_counter = Order_input.hierarchical_counter ()
	and appendix_counter = Order_input.hierarchical_counter ()
	and printout_counter = Order_input.ordinal_counter ()
	and equation_counter = Order_input.ordinal_counter ()
	and figure_counter = Order_input.ordinal_counter ()
	and table_counter = Order_input.ordinal_counter ()
	and bib_counter = Order_input.ordinal_counter ()
	and note_counter = Order_input.ordinal_counter ()
	and custom_counters = Hashtbl.create 10
        and auto_label_counter = ref 0
	and extinl_counter = ref 0
	and extblk_counter = ref 0
	and appendixed = ref false in


	(************************************************************************)
	(* Helper sub-functions.						*)
	(************************************************************************)

	(*	This subfunction creates a new label.  It checks whether the user
		explicitly provided a label (in which case we use the [User] variant),
		or if no label was defined (in which case we automatically assign a
		label using the [Auto] variant).
	*)
	let make_label comm target = match comm.comm_label with
		| Some thing ->
			let new_label = Label.User thing in
			begin
				if Identifier_input.matches_label thing
				then
					if Hashtbl.mem labels new_label
					then BatDynArray.add errors (Some comm.comm_linenum, (Error.Duplicate_target (comm.comm_tag, thing)))
					else Hashtbl.add labels new_label target
				else
					if thing <> ""
					then BatDynArray.add errors (Some comm.comm_linenum, (Error.Invalid_label (comm.comm_tag, thing)))
			end;
			new_label
		| None ->
			incr auto_label_counter;
			Label.Auto (string_of_int !auto_label_counter)


	(*	This subfunction creates an user ordinal order, checking
		for exceptions and appending the error list if necessary.
	*)
	and make_user_ordinal comm str =
		try
			Order_input.user_ordinal str
		with
			Order_input.Invalid_order_format str ->
				let msg = Error.Invalid_order_format (comm.comm_tag, str) in
				BatDynArray.add errors (Some comm.comm_linenum, msg);
				Order_input.user_ordinal "0"


	(*	This subfunction creates an user hierarchical order, checking
		for exceptions and appending the error list if necessary.
	*)
	and make_user_hierarchical comm level str =
		try
			Order_input.user_hierarchical level str
		with
			| Order_input.Invalid_order_format str ->
				let msg = Error.Invalid_order_format (comm.comm_tag, str) in
				BatDynArray.add errors (Some comm.comm_linenum, msg);
				Order_input.user_hierarchical (Level.section 1) "0"
			| Order_input.Invalid_order_levels (str, expected, found) ->
				let msg = Error.Invalid_order_levels (comm.comm_tag, str, expected, found) in
				BatDynArray.add errors (Some comm.comm_linenum, msg);
				Order_input.user_hierarchical (Level.section 1) "0"


	(*	Adds a new pointer to the dictionary.
	*)
	and add_pointer target_checker comm pointer =
		if Identifier_input.matches_label pointer
		then
			BatDynArray.add pointers (target_checker, comm, pointer)
		else
			let msg = Error.Invalid_label (comm.comm_tag, pointer) in
			BatDynArray.add errors (Some comm.comm_linenum, msg)


	(*	Adds a new TOC entry.
	*)
	and add_toc_entry heading =
		BatDynArray.add toc heading


	(*	Checker for inline/block commands.
	*)
	and check_comm ?maybe_minipaged ?maybe_wrapped feature comm elem =
		let check_classname classname =
			if not (Permission.check_classname feature classname idiosyncrasies)
			then 
				let msg = Error.Invalid_style_misplaced_classname (comm.comm_tag, classname) in
				BatDynArray.add errors (Some comm.comm_linenum, msg) in
		if Permission.check_feature feature idiosyncrasies
		then begin
			Permission.check_parameters ?maybe_minipaged ?maybe_wrapped errors comm feature;
			let (attr, style_parsing) = Style.parse comm errors in
			List.iter check_classname attr;
			let element = elem attr style_parsing in
			if Style.dispose comm errors style_parsing
			then Some element
			else None
		end else
			let msg = Error.Unavailable_feature (comm.comm_tag, Feature.describe feature) in
			BatDynArray.add errors (Some comm.comm_linenum, msg);
			None in


	(************************************************************************)
	(* Compilation functions for mathematics.				*)
	(************************************************************************)

	let convert_mathtex constructor comm mathtex =
		try
			[constructor (Math_input.from_mathtex mathtex)]
		with _ ->
			let msg = Error.Invalid_mathtex (comm.comm_tag, mathtex) in
			BatDynArray.add errors (Some comm.comm_linenum, msg);
			[]


	and convert_mathml constructor comm mathml =
		try
			[constructor (Math_input.from_mathml mathml)]
		with _ ->
			let msg = Error.Invalid_mathml (comm.comm_tag, mathml) in
			BatDynArray.add errors (Some comm.comm_linenum, msg);
			[] in


	(************************************************************************)
	(* Compilation functions for inline context.				*)
	(************************************************************************)

	let dummy_inline = Inline.linebreak () in


	let check_inline_comm ?maybe_minipaged ?maybe_wrapped feature comm elem =
		match check_comm ?maybe_minipaged ?maybe_wrapped feature comm elem with
			| Some x -> x
			| None	 -> [dummy_inline] in


	let rec convert_inline ~context ~depth ~args is_ref (comm, inline) = match inline with

		| Ast.Plain ustr ->
			let elem attr _ = [Inline.plain ~attr ustr] in
			check_inline_comm `Feature_plain comm elem

		| Ast.Entity ent ->
			let elem attr _ = match Entity_input.expand ent with
				| `Okay (txt, ustr) -> if expand_entities then [Inline.plain ~attr (ustr :> string)] else [Inline.entity ~attr txt]
				| `Error msg	    -> BatDynArray.add errors (Some comm.comm_linenum, msg); [] in
			check_inline_comm `Feature_entity comm elem

		| Ast.Linebreak ->
			let elem attr _ = [Inline.linebreak ~attr ()] in
			check_inline_comm `Feature_linebreak comm elem

		| Ast.Mathtex_inl txt ->
			let elem attr _ = convert_mathtex (Inline.mathinl ~attr) comm txt in
			check_inline_comm `Feature_mathtex_inl comm elem

		| Ast.Mathml_inl txt ->
			let elem attr _ = convert_mathml (Inline.mathinl ~attr) comm txt in
			check_inline_comm `Feature_mathml_inl comm elem

		| Ast.Glyph (href, alt) ->
			let elem attr _ =
				BatDynArray.add imagerefs (comm, `Feature_glyph, href);
				imageset := HrefSet.add href !imageset;
				[Inline.glyph ~attr href alt] in
			check_inline_comm `Feature_glyph comm elem

		| Ast.Bold astseq ->
			let elem attr _ = [Inline.bold ~attr (convert_seq_aux ~comm ~context ~depth ~args is_ref astseq)] in
			check_inline_comm `Feature_bold comm elem

		| Ast.Emph astseq ->
			let elem attr _ = [Inline.emph ~attr (convert_seq_aux ~comm ~context ~depth ~args is_ref astseq)] in
			check_inline_comm `Feature_emph comm elem

		| Ast.Code astseq ->
			let elem attr _ = [Inline.code ~attr (convert_seq_aux ~comm ~context ~depth ~args is_ref astseq)] in
			check_inline_comm `Feature_code comm elem

		| Ast.Caps astseq ->
			let elem attr _ = [Inline.caps ~attr (convert_seq_aux ~comm ~context ~depth ~args is_ref astseq)] in
			check_inline_comm `Feature_caps comm elem

		| Ast.Ins astseq ->
			let elem attr _ = [Inline.ins ~attr (convert_seq_aux ~comm ~context ~depth ~args is_ref astseq)] in
			check_inline_comm `Feature_ins comm elem

		| Ast.Del astseq ->
			let elem attr _ = [Inline.del ~attr (convert_seq_aux ~comm ~context ~depth ~args is_ref astseq)] in
			check_inline_comm `Feature_del comm elem

		| Ast.Sup astseq ->
			let elem attr _ = [Inline.sup ~attr (convert_seq_aux ~comm ~context ~depth ~args is_ref astseq)] in
			check_inline_comm `Feature_sup comm elem

		| Ast.Sub astseq ->
			let elem attr _ = [Inline.sub ~attr (convert_seq_aux ~comm ~context ~depth ~args is_ref astseq)] in
			check_inline_comm `Feature_sub comm elem

		| Ast.Mbox astseq ->
			let elem attr _ = [Inline.mbox ~attr (convert_seq_aux ~comm ~context ~depth ~args is_ref astseq)] in
			check_inline_comm `Feature_mbox comm elem

		| Ast.Span astseq ->
			let elem attr _ = [Inline.span ~attr (convert_seq_aux ~comm ~context ~depth ~args is_ref astseq)] in
			check_inline_comm `Feature_span comm elem

		| Ast.Link (href, maybe_astseq) when not is_ref ->
			let elem attr _ =
				BatDynArray.add linkrefs (comm, `Feature_link, href);
				linkset := HrefSet.add href !linkset;
				let maybe_seq = maybe (convert_seq_aux ~comm ~context ~depth ~args true) maybe_astseq in
				[Inline.link ~attr href maybe_seq] in
			check_inline_comm `Feature_link comm elem

		| Ast.See refs when not is_ref ->
			let elem attr _ =
				let target_checker = function
					| Target.Note_target _	-> `Valid_target
					| _			-> `Wrong_target Error.Target_note in
				List.iter (add_pointer target_checker comm) refs;
				match refs with
					| _::_ ->
						[Inline.see ~attr refs]
					| [] ->
						let msg = Error.Empty_list comm.comm_tag in
						BatDynArray.add errors (Some comm.comm_linenum, msg);
						[] in
			check_inline_comm `Feature_see comm elem

		| Ast.Cite refs when not is_ref ->
			let elem attr _ =
				let target_checker = function
					| Target.Bib_target _	-> `Valid_target
					| _			-> `Wrong_target Error.Target_bib in
				List.iter (add_pointer target_checker comm) refs;
				match refs with
					| _::_ ->
						[Inline.cite ~attr refs]
					| [] ->
						let msg = Error.Empty_list comm.comm_tag in
						BatDynArray.add errors (Some comm.comm_linenum, msg);
						[] in
			check_inline_comm `Feature_cite comm elem

		| Ast.Dref (pointer, maybe_astseq) when not is_ref ->
			let elem attr _ =
				let target_checker = function
					| Target.Visible_target (Target.Custom_target (_, _, `None_given))
					| Target.Visible_target (Target.Wrapper_target (_, `None_given))
					| Target.Visible_target (Target.Part_target `None_given)
					| Target.Visible_target (Target.Section_target (_, `None_given)) -> `Empty_target
					| Target.Visible_target _					 -> `Valid_target
					| _								 -> `Wrong_target Error.Target_label in
				add_pointer target_checker comm pointer;
				let maybe_seq = maybe (convert_seq_aux ~comm ~context ~depth ~args true) maybe_astseq in
				[Inline.dref ~attr pointer maybe_seq] in
			check_inline_comm `Feature_dref comm elem

		| Ast.Sref (pointer, maybe_astseq) when not is_ref ->
			let elem attr _ =
				let target_checker = function
					| Target.Visible_target (Target.Custom_target (_, _, `None_given))
					| Target.Visible_target (Target.Wrapper_target (_, `None_given))
					| Target.Visible_target (Target.Part_target `None_given)
					| Target.Visible_target (Target.Section_target (_, `None_given)) -> `Empty_target
					| Target.Visible_target _					 -> `Valid_target
					| _								 -> `Wrong_target Error.Target_label in
				add_pointer target_checker comm pointer;
				let maybe_seq = maybe (convert_seq_aux ~comm ~context ~depth ~args true) maybe_astseq in
				[Inline.sref ~attr pointer maybe_seq] in
			check_inline_comm `Feature_sref comm elem

		| Ast.Mref (pointer, astseq) when not is_ref ->
			let elem attr _ =
				let target_checker = function
					| Target.Visible_target _ -> `Valid_target
					| _			  -> `Wrong_target Error.Target_label in
				add_pointer target_checker comm pointer;
				[Inline.mref ~attr pointer (convert_seq_aux ~comm ~context ~depth ~args true astseq)] in
			check_inline_comm `Feature_mref comm elem

		| Ast.Macroarg raw_num ->
			let elem attr _ = match args with
				| None ->
					let msg = Error.Invalid_macro_argument_context in
					BatDynArray.add errors (Some comm.comm_linenum, msg);
					[]
				| Some x ->
					try
						let num = (int_of_string raw_num) - 1 in
						List.at x num
					with
						| Failure _
						| Invalid_argument _ ->
							let msg = Error.Invalid_macro_argument_number (raw_num, List.length x) in
							BatDynArray.add errors (Some comm.comm_linenum, msg);
							[] in
			check_inline_comm `Feature_macroarg comm elem

		| Ast.Macrocall (name, arglist) ->
			let elem attr _ =
				try
					let (macro_nargs, macro_astseq) = Hashtbl.find macros name in
					if macro_nargs <> List.length arglist
					then
						let msg = Error.Invalid_macro_call (name, List.length arglist, macro_nargs) in
						BatDynArray.add errors (Some comm.comm_linenum, msg);
						[]
					else 
						let (context_comm, depth) = context in
						match idiosyncrasies.max_macro_depth with
							| None ->
								let context = (context_comm, depth+1) in
								let new_arglist = List.map (convert_inline_list ~comm ~context ~depth ~args is_ref) arglist in
								convert_inline_list ~comm ~context ~depth ~args:(Some new_arglist) is_ref macro_astseq
							| Some num when depth < num ->
								let context = (context_comm, depth+1) in
								let new_arglist = List.map (convert_inline_list ~comm ~context ~depth ~args is_ref) arglist in
								convert_inline_list ~comm ~context ~depth ~args:(Some new_arglist) is_ref macro_astseq
							| Some num ->
								let msg = Error.Excessive_macro_depth (comm.comm_tag, num) in
								BatDynArray.add errors (Some comm.comm_linenum, msg);
								[]
				with
					| Not_found ->
						let msg = Error.Undefined_macro (comm.comm_tag, name) in
						BatDynArray.add errors (Some comm.comm_linenum, msg);
						[] in
			check_inline_comm `Feature_macrocall comm elem

		| Ast.Extinl (tag, extinl) when not is_ref || not (snd (List.assoc tag extinldefs)) ->
			let elem attr _ =
				let extkey = !extinl_counter in
				incr extinl_counter;
				let extinl = convert_extinl ~comm ~context ~depth ~args is_ref extinl in
				BatDynArray.add extinlrefs (extkey, tag, comm, extinl);
				[Inline.extinl ~attr extkey] in
			check_inline_comm (`Feature_extinl tag) comm elem

		| _ ->
			let msg = Error.Unexpected_inline comm.comm_tag in
			BatDynArray.add errors (Some comm.comm_linenum, msg);
			[]


	and convert_extinl ~comm ~context ~depth ~args is_ref = function
		| Ast.Extinl_simseq astseq ->
			Extcomm.Extinl_simseq (convert_seq_aux ~comm ~context ~depth ~args is_ref astseq)
		| Ast.Extinl_simraw txt ->
			Extcomm.Extinl_simraw txt
		| Ast.Extinl_simrawseq (txt, astseq) ->
			Extcomm.Extinl_simrawseq (txt, convert_seq_aux ~comm ~context ~depth ~args is_ref astseq)
		| Ast.Extinl_simrawseqopt (txt, maybe_astseq) ->
			Extcomm.Extinl_simrawseqopt (txt, maybe (convert_seq_aux ~comm ~context ~depth ~args is_ref) maybe_astseq)


	and convert_inline_list ~comm ~context ~depth ~args is_ref astseq = match idiosyncrasies.max_inline_depth with
		| Some max when depth >= max ->
			let msg = Error.Excessive_inline_depth (comm.comm_tag, max) in
			BatDynArray.add errors (Some comm.comm_linenum, msg);
			[dummy_inline]
		| _ ->
			let coalesce_plain seq =
				let rec coalesce_plain_aux accum = function
					| {Inline.inline = Inline.Plain txt1; attr} :: {Inline.inline = Inline.Plain txt2; _} :: tl ->
						let agg = Inline.plain ~attr (txt1 ^ txt2) in
						coalesce_plain_aux accum (agg :: tl)
					| hd :: tl ->
						coalesce_plain_aux (hd :: accum) tl
					| [] ->
						accum in
				List.rev (coalesce_plain_aux [] seq) in
			let seq = flatten_map (convert_inline ~context ~depth:(depth+1) ~args is_ref) astseq in
			let new_seq = if macros_authorised || expand_entities then coalesce_plain seq else seq in
			match new_seq with
				| [] ->
					let msg = Error.Empty_sequence comm.comm_tag in
					BatDynArray.add errors (Some comm.comm_linenum, msg);
					[dummy_inline]
				| xs ->
					xs


	and convert_seq_aux ~comm ~context ~depth ~args is_ref astseq =
		convert_inline_list ~comm ~context ~depth ~args is_ref astseq in


	let convert_seq ~comm ?args seq =
		convert_seq_aux ~comm ~context:(comm, 0) ~depth:0 ~args false seq in


	(************************************************************************)
	(* Compilation functions for tabular environment.			*)
	(************************************************************************)

	let convert_tabular comm tcols tab =

		let get_colspec comm spec =
			try
				Tabular_input.colspec_of_string spec
			with
				Invalid_argument _ ->
					let msg = Error.Invalid_column_specifier (comm.comm_tag, spec) in
					BatDynArray.add errors (Some comm.comm_linenum, msg);
					(Tabular.Center, Tabular.Normal) in

		let specs = Array.map (get_colspec comm) (Array.of_list (List.map String.of_char (String.explode tcols))) in

		let num_columns = Array.length specs in

		let convert_cell (comm, raw_cellspec, maybe_astseq) =
			let (colspan, cellspec) = match raw_cellspec with
				| Some raw ->
					begin
						try
							let (colspec, colspan, overline, underline) = Tabular_input.cellspec_of_string raw in
							(colspan, Some (colspec, colspan, overline, underline))
						with _ ->
							let msg = Error.Invalid_cell_specifier (comm.comm_tag, raw) in
							BatDynArray.add errors (Some comm.comm_linenum, msg);
							(1, None)
					end
				| None ->
					(1, None) in
			(colspan, Tabular.make_cell cellspec (maybe (convert_seq ~comm) maybe_astseq)) in

		let convert_row (row_comm, row) =
			let rowspan = ref 0 in
			let converter raw_cell =
				let (colspan, cell) = convert_cell raw_cell in
				let () = rowspan := !rowspan + colspan in
				cell in
			let tab_row = match row with
				| _::_ -> Tabular.make_row (List.map converter row)
				| []   -> invalid_arg "Parser has given us an empty tabular row" in
			if !rowspan <> num_columns
			then begin
				let msg = Error.Invalid_column_number (row_comm.comm_tag, comm.comm_tag, comm.comm_linenum, !rowspan, num_columns) in
				BatDynArray.add errors (Some row_comm.comm_linenum, msg);
				tab_row
			end else
				tab_row in

		let convert_group feature (maybe_comm, rows) =
			let () = match maybe_comm with
				| Some comm	-> ignore (check_inline_comm feature comm (fun _ _ -> []))
				| None		-> () in
			 match rows with
				| _::_ -> Tabular.make_group (List.map convert_row rows)
				| []   -> invalid_arg "Parser has given us an empty tabular group" in

		let thead = maybe (convert_group `Feature_thead) tab.thead in

		let tfoot = maybe (convert_group `Feature_tfoot) tab.tfoot in

		match tab.tbodies with
			| _::_ -> Tabular.make specs ?thead ?tfoot (List.map (convert_group `Feature_tbody) tab.tbodies)
			| []   -> invalid_arg "Parser has given us an empty tabular body" in


	(************************************************************************)
	(* Compilation functions for document blocks.				*)
	(************************************************************************)

	let dummy_block = Block.paragraph [dummy_inline] in


	let check_block_comm ?maybe_minipaged ?maybe_wrapped feature comm elem =
		match check_comm ?maybe_minipaged ?maybe_wrapped feature comm elem with
			| Some x -> x
			| None	 -> [dummy_block] in


	let rec convert_block ~minipaged ~depth allowed (comm, astblk) = match astblk with

		| Ast.Paragraph astseq when allowed = `Paragraph_blk || Blkcat.(`Embeddable_blk <: allowed) ->
			let elem attr _ = [Block.paragraph ~attr (convert_seq ~comm astseq)] in
			check_block_comm `Feature_paragraph comm elem

		| Ast.Itemize astfrags when Blkcat.(`Embeddable_blk <: allowed) ->
			let elem attr _ = convert_frag_of_anon_frags ~comm ~cons:(Block.itemize ~attr) ~minipaged ~depth allowed astfrags in
			check_block_comm `Feature_itemize comm elem

		| Ast.Enumerate astfrags when Blkcat.(`Embeddable_blk <: allowed) ->
			let elem attr _ = convert_frag_of_anon_frags ~comm ~cons:(Block.enumerate ~attr) ~minipaged ~depth allowed astfrags in
			check_block_comm `Feature_enumerate comm elem

		| Ast.Description astfrags when Blkcat.(`Embeddable_blk <: allowed) ->
			let elem attr _ = convert_frag_of_desc_frags ~comm ~cons:(Block.description ~attr) ~minipaged ~depth allowed astfrags in
			check_block_comm `Feature_description comm elem

		| Ast.Qanda astfrags when Blkcat.(`Embeddable_blk <: allowed) ->
			let elem attr _ = convert_frag_of_qanda_frags ~comm ~cons:(Block.qanda ~attr) ~minipaged ~depth allowed astfrags in
			check_block_comm `Feature_qanda comm elem

		| Ast.Verse astfrag when Blkcat.(`Embeddable_blk <: allowed) ->
			let elem attr _ = [Block.verse ~attr (convert_frag_aux ~comm ~minipaged ~depth `Paragraph_blk astfrag)] in
			check_block_comm `Feature_verse comm elem

		| Ast.Quote astfrag when Blkcat.(`Quotable_blk <: allowed) ->
			let elem attr _ = [Block.quote ~attr (convert_frag_aux ~comm ~minipaged ~depth `Quotable_blk astfrag)] in
			check_block_comm `Feature_quote comm elem

		| Ast.Mathtex_blk txt when allowed = `Equation_blk || Blkcat.(`Embeddable_blk <: allowed) ->
			let elem attr _ = convert_mathtex (Block.mathblk ~attr) comm txt in
			check_block_comm `Feature_mathtex_blk comm elem

		| Ast.Mathml_blk txt when allowed = `Equation_blk || Blkcat.(`Embeddable_blk <: allowed) ->
			let elem attr _ = convert_mathml (Block.mathblk ~attr) comm txt in
			check_block_comm `Feature_mathml_blk comm elem

		| Ast.Source txt when allowed = `Printout_blk || Blkcat.(`Embeddable_blk <: allowed) ->
			let elem attr dict =
				let (lang, linenums) = Style.consume2 dict (Lang_hnd, None) (Linenums_hnd, false) in
				let trimmed = Literal_input.trim txt in
				let hilite = Camlhighlight_parser.from_string ?lang trimmed in
				let src = Source.make lang hilite linenums in
				let () = if trimmed = "" then BatDynArray.add errors (Some comm.comm_linenum, Error.Empty_source comm.comm_tag) in
				[Block.source ~attr src] in
			check_block_comm `Feature_source comm elem

		| Ast.Tabular (tcols, tab) when allowed = `Table_blk || Blkcat.(`Embeddable_blk <: allowed) ->
			let elem attr _ = [Block.tabular ~attr (convert_tabular comm tcols tab)] in
			check_block_comm `Feature_tabular comm elem

		| Ast.Subpage astfrag when allowed = `Figure_blk || Blkcat.(`Listable_blk <: allowed) ->
			let elem attr _ = [Block.subpage ~attr (convert_frag_aux ~comm ~minipaged:true ~depth `Super_blk astfrag)] in
			check_block_comm `Feature_subpage comm elem

		| Ast.Verbatim txt when allowed = `Figure_blk || Blkcat.(`Embeddable_blk <: allowed) ->
			let elem attr _ =
				let trimmed = Literal_input.trim txt in
				let () = if trimmed = "" then BatDynArray.add errors (Some comm.comm_linenum, Error.Empty_verbatim comm.comm_tag) in
				[Block.verbatim ~attr trimmed] in
			check_block_comm `Feature_verbatim comm elem

		| Ast.Picture (href, alt) when allowed = `Figure_blk || Blkcat.(`Embeddable_blk <: allowed) ->
			let elem attr dict =
				BatDynArray.add imagerefs (comm, `Feature_picture, href);
				imageset := HrefSet.add href !imageset;
				let width = Style.consume1 dict (Width_hnd, None) in
				[Block.picture ~attr href alt width] in
			check_block_comm `Feature_picture comm elem

		| Ast.Pullquote (maybe_astseq, astfrag) when Blkcat.(`Listable_blk <: allowed) ->
			let elem attr _ =
				let maybe_seq = maybe (convert_seq ~comm) maybe_astseq in
				let frag = convert_frag_aux ~comm ~minipaged ~depth `Embeddable_blk astfrag in
				[Block.pullquote ~attr maybe_seq frag] in
			check_block_comm `Feature_pullquote comm elem

		| Ast.Custom (maybe_kind, env, maybe_astseq, astfrag) when Blkcat.(`Listable_blk <: allowed) ->
			let elem attr _ =
				let bad_order reason =
					let msg = Error.Misplaced_order_parameter (comm.comm_tag, reason) in
					BatDynArray.add errors (Some comm.comm_linenum, msg);
					Order_input.no_order () in
				try
					let (kind, used, def) = Hashtbl.find customisations env in
					let () = match maybe_kind with Some k when k <> kind -> raise (Mismatched_custom (k, kind)) | _ -> () in
					let () = if not used then Hashtbl.replace customisations env (kind, true, def) in
					let order = match (def, comm.comm_order, minipaged) with
						| Numbered _, None, true	     -> bad_order Error.Reason_is_absent_when_mandatory
						| Numbered (_, counter), None, false -> Order_input.auto_ordinal counter
						| Numbered _, Some "", _	     -> Order_input.no_order ()
						| Numbered _ , Some other, true	     -> make_user_ordinal comm other
						| Numbered _ , Some other, false     -> bad_order (Error.Reason_is_non_empty_when_forbidden other)
						| _, None, _			     -> Order_input.no_order ()
						| _, Some "", _			     -> Order_input.no_order ()
						| _, Some other, _		     -> bad_order (Error.Reason_is_non_empty_when_forbidden other) in
					let label = make_label comm (Target.custom env kind order) in
					let custom_maker = match def with
						| Anonymous    -> Custom.anonymous
						| Unnumbered _ -> Custom.unnumbered
						| Numbered _   -> Custom.numbered in
					let data = custom_maker env label order in
					let (block_maker, allowed) = match kind with
						| Custom.Boxout  -> (Block.boxout ~attr (Custom.Boxout.make data), Blkcat.min allowed `Quotable_blk)
						| Custom.Theorem -> (Block.theorem ~attr (Custom.Theorem.make data), `Embeddable_blk) in
					let maybe_seq = maybe (convert_seq ~comm) maybe_astseq in
					let frag = convert_frag_aux ~comm ~minipaged ~depth allowed astfrag in
					[block_maker maybe_seq frag]
				with
					| Not_found ->
						let msg = Error.Undefined_custom (comm.comm_tag, env) in
						BatDynArray.add errors (Some comm.comm_linenum, msg);
						[]
					| Mismatched_custom (found, expected) ->
						let msg = Error.Mismatched_custom (comm.comm_tag, env, found, expected) in
						BatDynArray.add errors (Some comm.comm_linenum, msg);
						[] in
			check_block_comm ~maybe_minipaged:(Some minipaged) `Feature_custom comm elem

		| Ast.Equation (maybe_astseq, astblk) when Blkcat.(`Listable_blk <: allowed) ->
			let elem attr _ =
				let wrapper = convert_wrapper comm equation_counter Wrapper.Equation maybe_astseq in
				let blk = convert_block ~minipaged ~depth `Equation_blk astblk in
				perhaps (Block.equation ~attr wrapper) blk in
			check_block_comm ~maybe_minipaged:(Some minipaged) `Feature_equation comm elem

		| Ast.Printout (maybe_astseq, astblk) when Blkcat.(`Listable_blk <: allowed) ->
			let elem attr _ =
				let wrapper = convert_wrapper comm printout_counter Wrapper.Printout maybe_astseq in
				let blk = convert_block ~minipaged ~depth `Printout_blk astblk in
				perhaps (Block.printout ~attr wrapper) blk in
			check_block_comm ~maybe_minipaged:(Some minipaged) `Feature_printout comm elem

		| Ast.Table (maybe_astseq, astblk) when Blkcat.(`Listable_blk <: allowed) ->
			let elem attr _ =
				let wrapper = convert_wrapper comm table_counter Wrapper.Table maybe_astseq in
				let blk = convert_block ~minipaged ~depth `Table_blk astblk in
				perhaps (Block.table ~attr wrapper) blk in
			check_block_comm ~maybe_minipaged:(Some minipaged) `Feature_table comm elem

		| Ast.Figure (maybe_astseq, astblk) when Blkcat.(`Listable_blk <: allowed) ->
			let elem attr _ =
				let wrapper = convert_wrapper comm figure_counter Wrapper.Figure maybe_astseq in
				let blk = convert_block ~minipaged ~depth `Figure_blk astblk in
				perhaps (Block.figure ~attr wrapper) blk in
			check_block_comm ~maybe_minipaged:(Some minipaged) `Feature_figure comm elem

		| Ast.Part astseq when allowed = `Super_blk ->
			let elem attr _ =
				let order = match comm.comm_order with
					| None	     -> Order_input.auto_ordinal part_counter
					| Some ""    -> Order_input.no_order ()
					| Some other -> make_user_ordinal comm other in
				let label = make_label comm (Target.part order) in
				let heading = Heading.part label order (convert_seq ~comm astseq) in
				let block = Block.heading ~attr heading in
				let () = if not minipaged then add_toc_entry heading in
				[block] in
			check_block_comm ~maybe_minipaged:(Some minipaged) `Feature_part comm elem

		| Ast.Appendix when allowed = `Super_blk ->
			let elem attr _ =
				let order = Order_input.no_order () in
				let label = make_label comm (Target.part order) in
				let heading = Heading.appendix label in
				let block = Block.heading ~attr heading in
				let () = if not minipaged then add_toc_entry heading in
				let () = appendixed := true in
				[block] in
			check_block_comm ~maybe_minipaged:(Some minipaged) `Feature_appendix comm elem

		| Ast.Section (level, astseq) when allowed = `Super_blk ->
			let elem attr _ =
				let (counter, location) =
					if !appendixed
					then (appendix_counter, Heading.Appendixed)
					else (section_counter, Heading.Mainbody) in
				let level' =
					try Level.section level
					with Invalid_argument _ -> 
						let msg = Error.Invalid_section_level (comm.comm_tag, level) in
						BatDynArray.add errors (Some comm.comm_linenum, msg);
						Level.section 1 in
				let order = match comm.comm_order with
					| None	     -> Order_input.auto_hierarchical level' counter
					| Some ""    -> Order_input.no_order ()
					| Some other -> make_user_hierarchical comm level' other in
				let label = make_label comm (Target.section location order) in
				let heading = Heading.section label order location level' (convert_seq ~comm astseq) in
				let block = Block.heading ~attr heading in
				let () = if not minipaged then add_toc_entry heading in
				[block] in
			let feature = match level with
				| 1 -> `Feature_section1
				| 2 -> `Feature_section2
				| 3 -> `Feature_section3
				| 4 -> `Feature_section4
				| 5 -> `Feature_section5
				| _ -> `Feature_section6 in
			check_block_comm ~maybe_minipaged:(Some minipaged) feature comm elem

		| Ast.Bibliography when allowed = `Super_blk ->
			convert_preset_sectional ~tocable:true ~minipaged Heading.bibliography `Feature_bibliography comm

		| Ast.Notes when allowed = `Super_blk ->
			convert_preset_sectional ~tocable:true ~minipaged Heading.notes `Feature_notes comm 

		| Ast.Toc when allowed = `Super_blk ->
			convert_preset_sectional ~tocable:false ~minipaged Heading.toc `Feature_toc comm

		| Ast.Title (level, astseq) when allowed = `Super_blk  ->
			let elem attr _ =
				let level' =
					try Level.title level
					with Invalid_argument _ -> 
						let msg = Error.Invalid_title_level (comm.comm_tag, level) in
						BatDynArray.add errors (Some comm.comm_linenum, msg);
						Level.title 1 in
				[Block.title ~attr level' (convert_seq ~comm astseq)] in
			let feature = match level with
				| 1 -> `Feature_title1
				| _ -> `Feature_title2 in
			check_block_comm feature comm elem

		| Ast.Abstract astfrag when allowed = `Super_blk ->
			let elem attr _ =
				let frag = convert_frag_aux ~comm ~minipaged ~depth `Embeddable_blk astfrag in
				[Block.abstract ~attr frag] in
			check_block_comm `Feature_abstract comm elem

		| Ast.Rule when allowed = `Super_blk ->
			let elem attr _ = [Block.rule ~attr ()] in
			check_block_comm `Feature_rule comm elem

		| Ast.Bib bib ->
			let elem attr _ =
				let (author_comm, author_astseq) = bib.author
				and (title_comm, title_astseq) = bib.title
				and (resource_comm, resource_astseq) = bib.resource in
				let order = Order_input.auto_ordinal bib_counter in
				let label = make_label comm (Target.bib order)
				and author =
					let elem attr _ = convert_seq ~comm author_astseq in
					check_comm `Feature_bib_author author_comm elem
				and title =
					let elem attr _ = convert_seq ~comm title_astseq in
					check_comm `Feature_bib_title title_comm elem
				and resource =
					let elem attr _ = convert_seq ~comm resource_astseq in
					check_comm `Feature_bib_resource resource_comm elem in
				match (author, title, resource) with
					| (Some author, Some title, Some resource) ->
						let bib = Bib.make label order author title resource in
						BatDynArray.add bibs bib;
						[]
					| _ ->
						[] in
			check_block_comm `Feature_bib comm elem

		| Ast.Note astfrag ->
			let elem attr _ =
				let order = Order_input.auto_ordinal note_counter in
				let label = make_label comm (Target.note order) in
				let frag = convert_frag_aux ~comm ~minipaged:true ~depth `Listable_blk astfrag in
				let note = Note.make label order frag in
				BatDynArray.add notes note;
				[] in
			check_block_comm `Feature_note comm elem

		| Ast.Macrodef (name, nargs, astseq) ->
			let elem attr _ =
				if not (Identifier_input.matches_macrodef name)
				then begin
					let msg = Error.Invalid_macro (comm.comm_tag, name) in
					BatDynArray.add errors (Some comm.comm_linenum, msg)
				end;
				let num_args =
					try int_of_string nargs
					with Failure _ ->
						let msg = Error.Invalid_macro_nargs (name, nargs) in
						BatDynArray.add errors (Some comm.comm_linenum, msg); 0 in
				let errors_before = BatDynArray.length errors in
				let _seq = convert_seq ~comm ~args:(List.make num_args [dummy_inline]) astseq in
				let errors_after = BatDynArray.length errors in
				(if Hashtbl.mem macros name
				then
					let msg = Error.Duplicate_macro (comm.comm_tag, name) in
					BatDynArray.add errors (Some comm.comm_linenum, msg)
				else
					let new_astseq = if errors_after = errors_before then astseq else [(comm, Ast.Linebreak)] in
					Hashtbl.add macros name (num_args, new_astseq));
				[] in
			check_block_comm `Feature_macrodef comm elem

		| Ast.Boxoutdef (env, maybe_caption, maybe_counter_name) ->
			let elem attr _ = convert_customdef comm env Custom.Boxout maybe_caption maybe_counter_name; [] in
			check_block_comm `Feature_boxoutdef comm elem

		| Ast.Theoremdef (env, caption, maybe_counter_name) ->
			let elem attr _ = convert_customdef comm env Custom.Theorem (Some caption) maybe_counter_name; [] in
			check_block_comm `Feature_theoremdef comm elem

		| Ast.Extblk (tag, extblk) when List.exists Blkcat.(fun blk -> blk = allowed || blk <: allowed) (snd (List.assoc tag extblkdefs)) ->
			let elem attr _ =
				let extkey = !extblk_counter in
				incr extblk_counter;
				let extblk = convert_extblk ~comm extblk in
				BatDynArray.add extblkrefs (extkey, tag, comm, extblk);
				[Block.extblk ~attr extkey] in
			check_block_comm (`Feature_extblk tag) comm elem

		| _ ->
			let msg = Error.Unexpected_block (comm.comm_tag, allowed) in
			BatDynArray.add errors (Some comm.comm_linenum, msg);
			[dummy_block]


	and convert_preset_sectional ~tocable ~minipaged cons feature comm = 
		let elem attr _ =
			let order = Order_input.no_order () in
			let label = make_label comm (Target.section Heading.Mainbody order) in
			let heading = cons label in
			let block = Block.heading ~attr heading in
			let () = if tocable && not minipaged then add_toc_entry heading in
			[block] in
		check_block_comm ~maybe_minipaged:(Some minipaged) `Feature_notes comm elem


	and convert_wrapper comm counter kind maybe_astseq =
		let order = match comm.comm_order with
			| None	     -> Order_input.auto_ordinal counter
			| Some ""    -> Order_input.no_order ()
			| Some thing -> make_user_ordinal comm thing in
		let label = make_label comm (Target.wrapper kind order) in
		let maybe_seq = maybe (convert_seq ~comm) maybe_astseq in
		match (order, maybe_seq) with
			| (`None_given, None) ->
				let msg = Error.Invalid_wrapper (comm.comm_tag, kind) in
				BatDynArray.add errors (Some comm.comm_linenum, msg);
				Wrapper.Unordered (label, [dummy_inline])
			| (`None_given, Some seq) ->
				Wrapper.Unordered (label, seq)
			| (`Auto_given _ as o, maybe_seq)
			| (`User_given _ as o, maybe_seq) ->
				Wrapper.Ordered (label, o, maybe_seq)


	and convert_customdef comm env kind maybe_caption maybe_counter_name =
		if not (Identifier_input.matches_customdef env)
		then begin
			let msg = Error.Invalid_custom (comm.comm_tag, env) in
			BatDynArray.add errors (Some comm.comm_linenum, msg)
		end
		else if Hashtbl.mem customisations env
		then begin
			let msg = Error.Duplicate_custom (comm.comm_tag, env) in
			BatDynArray.add errors (Some comm.comm_linenum, msg)
		end
		else match (maybe_caption, maybe_counter_name) with
			| (None, None) ->
				let data = (kind, false, Anonymous) in
				Hashtbl.add customisations env data
			| (None, Some counter_name) ->
				let msg = Error.Unexpected_counter (comm.comm_tag, counter_name) in
				BatDynArray.add errors (Some comm.comm_linenum, msg)
			| (Some astseq, None) ->
				let data = (kind, false, Unnumbered (convert_seq ~comm astseq)) in
				Hashtbl.add customisations env data
			| (Some astseq, Some counter_name) when not (Hashtbl.mem custom_counters counter_name) ->
				if Identifier_input.matches_counter counter_name
				then begin
					let counter = Order_input.ordinal_counter () in
					let data = (kind, false, Numbered (convert_seq ~comm astseq, counter)) in
					Hashtbl.add custom_counters counter_name (kind, counter);
					Hashtbl.add customisations env data
				end
				else begin
					let msg = Error.Invalid_counter (comm.comm_tag, counter_name) in
					BatDynArray.add errors (Some comm.comm_linenum, msg)
				end
			| (Some astseq, Some counter_name) -> match Hashtbl.find custom_counters counter_name with
				| (k, _) when k <> kind ->
					let msg = Error.Mismatched_counter (comm.comm_tag, counter_name) in
					BatDynArray.add errors (Some comm.comm_linenum, msg)
				| (_, counter) ->
					let data = (kind, false, Numbered (convert_seq ~comm astseq, counter)) in
					Hashtbl.add customisations env data


	and convert_frag_of_anon_frags ~comm ~cons ~minipaged ~depth allowed astfrags =
		let conv (comm, astfrag) =
			let elem attr _ = convert_frag_aux ~comm ~minipaged ~depth (Blkcat.min allowed `Listable_blk) astfrag in
			check_comm `Feature_item comm elem in
		let frags = List.filter_map conv astfrags in
		match frags with
			| _::_ ->
				[cons frags]
			| [] ->
				let msg = Error.Empty_fragment comm.comm_tag in
				BatDynArray.add errors (Some comm.comm_linenum, msg);
				[dummy_block]


	and convert_frag_of_desc_frags ~comm ~cons ~minipaged ~depth allowed astfrags =
		let conv (comm, astseq, astfrag) =
			let elem attr _ =
				let seq = convert_seq ~comm astseq in
				let frag = convert_frag_aux ~comm ~minipaged ~depth (Blkcat.min allowed `Listable_blk) astfrag in
				(seq, frag) in
			check_comm `Feature_item comm elem in
		let frags = List.filter_map conv astfrags in
		match frags with
			| _::_ ->
				[cons frags]
			| [] ->
				let msg = Error.Empty_fragment comm.comm_tag in
				BatDynArray.add errors (Some comm.comm_linenum, msg);
				[dummy_block]


	and convert_frag_of_qanda_frags ~comm ~cons ~minipaged ~depth allowed astfrags =
		let conv (comm, qanda, astfrag) =
			let (feature, qanda_maker) = match qanda with
				| New_questioner maybe_astseq ->
					(`Feature_question, fun () -> Qanda.New_questioner (maybe (convert_seq ~comm) maybe_astseq))
				| New_answerer maybe_astseq ->
					(`Feature_answer, fun () -> Qanda.New_answerer (maybe (convert_seq ~comm) maybe_astseq))
				| Same_questioner ->
					(`Feature_rquestion, fun () -> Qanda.Same_questioner)
				| Same_answerer ->
					(`Feature_ranswer, fun () -> Qanda.Same_answerer) in
			let elem attr _ =
				let qanda = qanda_maker () in
				let frag = convert_frag_aux ~comm ~minipaged ~depth (Blkcat.min allowed `Listable_blk) astfrag in
				[(qanda, frag)] in
			check_comm feature comm elem in
		let frags = List.flatten (List.filter_map conv astfrags) in
		match frags with
			| _::_ ->
				[cons frags]
			| [] ->
				let msg = Error.Empty_fragment comm.comm_tag in
				BatDynArray.add errors (Some comm.comm_linenum, msg);
				[dummy_block]


	and convert_extblk ~comm = function
		| Ast.Extblk_simseq astseq			-> Extcomm.Extblk_simseq (convert_seq ~comm astseq)
		| Ast.Extblk_simraw txt				-> Extcomm.Extblk_simraw txt
		| Ast.Extblk_envraw txt				-> Extcomm.Extblk_envraw txt
		| Ast.Extblk_envseqraw (astseq, txt)		-> Extcomm.Extblk_envseqraw (convert_seq ~comm astseq, txt)
		| Ast.Extblk_envrawraw (txt1, txt2)		-> Extcomm.Extblk_envrawraw (txt1, txt2)
		| Ast.Extblk_envseqoptraw (maybe_astseq, txt)	-> Extcomm.Extblk_envseqoptraw (maybe (convert_seq ~comm) maybe_astseq, txt)
		| Ast.Extblk_envrawoptraw (maybe_txt1, txt2)	-> Extcomm.Extblk_envrawoptraw (maybe_txt1, txt2)


	and convert_frag_aux ?comm ~minipaged ~depth allowed astfrag =
		let conv = match idiosyncrasies.max_block_depth with
			| None -> fun astblk ->
				convert_block ~minipaged ~depth allowed astblk
			| Some max -> fun astblk ->
				if depth >= max
				then
					let (comm, _) = astblk in
					let msg = Error.Excessive_block_depth (comm.comm_tag, max) in
					BatDynArray.add errors (Some comm.comm_linenum, msg);
					[dummy_block]
				else
					convert_block ~minipaged ~depth:(depth+1) allowed astblk in
		let frag = flatten_map conv astfrag in
		match frag with
			| _::_ ->
				frag
			| [] ->
				let (tag, linenum) = match comm with
					| Some comm -> (comm.comm_tag, Some comm.comm_linenum)
					| None	    -> (None, None) in
				let msg = Error.Empty_fragment tag in
				BatDynArray.add errors (linenum, msg);
				[dummy_block] in


	let convert_frag astfrag =
		convert_frag_aux ~minipaged:false ~depth:0 `Super_blk astfrag in


	(************************************************************************)
	(* Filtering of internal references.					*)
	(************************************************************************)

	let filter_pointers () =
		let filter_pointer (target_checker, comm, label) =
			try
				let target = Hashtbl.find labels (Label.User label) in
				match target_checker target with
				| `Valid_target ->
					()
				| `Empty_target ->
					let msg = Error.Empty_target (comm.comm_tag, label) in
					BatDynArray.add errors (Some comm.comm_linenum, msg)
				| `Wrong_target expected ->
					let suggestion = match target with
						| Target.Visible_target _	-> Error.Target_label
						| Target.Bib_target _		-> Error.Target_bib
						| Target.Note_target _		-> Error.Target_note in
					let msg = Error.Wrong_target (comm.comm_tag, label, expected, suggestion) in
					BatDynArray.add errors (Some comm.comm_linenum, msg)
			with
				Not_found ->
					let msg = Error.Undefined_target (comm.comm_tag, label) in
					BatDynArray.add errors (Some comm.comm_linenum, msg) in
		BatDynArray.iter filter_pointer pointers in


	(************************************************************************)
	(* Filtering of customisations: we only save those actually used.	*)
	(************************************************************************)

	let filter_customisations () =
		let custom = Hashtbl.create (Hashtbl.length customisations) in
		let adder key = function
			| (_, true, Unnumbered seq)
			| (_, true, Numbered (seq, _)) -> Hashtbl.add custom key seq
			| _			       -> () in
		Hashtbl.iter adder customisations;
		custom in


	(************************************************************************)
	(* Resolve referenced links.						*)
	(************************************************************************)

	let process_href processor set refs =
		let dict = Hashtbl.create (HrefSet.cardinal set) in
		let aux href =
			processor ?rconfig href >>= fun v ->
			Monad.return (Hashtbl.add dict href v) in
		Monad.iter aux (HrefSet.elements set) >>= fun () ->
		let newdict = Hashtbl.create (BatDynArray.length refs) in
		let process (comm, feature, href) = match Hashtbl.find dict href with
			| `Okay v ->
				Hashtbl.add newdict href v
			| `Error error ->
				let msg = match error with
					| `Unsupported -> Error.Unsupported_extension comm.comm_tag
					| `Failed expl -> Error.Failed_extension (comm.comm_tag, expl)
					| `Style expl  -> Error.Invalid_style_extension (comm.comm_tag, expl) in
				BatDynArray.add errors (Some comm.comm_linenum, msg) in
		let () = BatDynArray.iter process refs in
		Monad.return newdict in


	(************************************************************************)
	(* Resolve command extensions.						*)
	(************************************************************************)

	let process_extcomm processor refs =
		let dict = Hashtbl.create (BatDynArray.length refs) in
		let aux (extkey, tag, comm, extcomm) =
			processor ?rconfig tag extcomm >>= function
				| `Okay v ->
					Monad.return (Hashtbl.add dict extkey (tag, extcomm, v))
				| `Error error ->
					let msg = match error with
						| `Unsupported -> Error.Unsupported_extension comm.comm_tag
						| `Failed expl -> Error.Failed_extension (comm.comm_tag, expl)
						| `Style expl  -> Error.Invalid_style_extension (comm.comm_tag, expl) in
					BatDynArray.add errors (Some comm.comm_linenum, msg);
					Monad.return () in
		Monad.iter aux (BatDynArray.to_list refs) >>= fun () ->
		Monad.return dict in


	(************************************************************************)
	(* Wrap-up.								*)
	(************************************************************************)

	let content = convert_frag ast in
	let customs = filter_customisations () in
	let () = filter_pointers () in
	process_href Ext.read_link !linkset linkrefs >>= fun links ->
	process_href Ext.read_image !imageset imagerefs >>= fun images ->
	process_extcomm Ext.read_extinl extinlrefs >>= fun extinls ->
	process_extcomm Ext.read_extblk extblkrefs >>= fun extblks ->
	Monad.return (content, BatDynArray.to_list bibs, BatDynArray.to_list notes, BatDynArray.to_list toc, labels, customs, links, images, extinls, extblks, BatDynArray.to_list errors)


(********************************************************************************)
(**	{2 Error processing}							*)
(********************************************************************************)

(**	Error collation function.  It takes a list of errors containing each an
	error message and an error line, and produces a proper error message
	where the actual source lines are displayed.
*)
let collate_errors =
	let rex = Pcre.regexp "\\r\\n|\\n" in
	fun source errors ->
		let source_lines = Pcre.asplit ~rex ~max:(-1) source in
		let num_lines = Array.length source_lines in
		let format_error (error_linenum, error_msg) =
			let error_context = match error_linenum with
				| Some num ->
					let num = max 1 (min num num_lines) in
					Some	{
						Error.error_line_number = num;
						Error.error_line_before = if num >= 2 then [source_lines.(num - 2)] else [];
						Error.error_line_actual = source_lines.(num - 1);
						Error.error_line_after = if num < num_lines then [source_lines.(num)] else []
						}
				| None ->
					None in
			(error_context, error_msg) in
		List.map format_error errors


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

let process_errors ~sort source errors =
	let compare (anum, amsg) (bnum, bmsg) = match (anum, bnum) with
		| (Some anum, Some bnum) ->
			let res = BatInt.compare anum bnum in
			if res = 0
			then Pervasives.compare amsg bmsg
			else res
		| (Some _, None) ->
			-1
		| (None, Some _) ->
			1
		| (None, None) ->
			Pervasives.compare amsg bmsg in
	let sorted = if sort then List.sort_unique compare errors else errors in
	let collated = collate_errors source sorted in
	match collated with
		| _::_ -> collated
		| []   -> assert false


let compile ?rconfig ~expand_entities ~idiosyncrasies ~source ast =
	compile_document ?rconfig ~expand_entities ~idiosyncrasies ast >>= fun (content, bibs, notes, toc, labels, customs, links, images, extinls, extblks, errors) ->
	match errors with
		| []   -> Monad.return (Ambivalent.make_valid ~content ~bibs ~notes ~toc ~labels ~customs ~links ~images ~extinls ~extblks)
		| _::_ -> Monad.return (Ambivalent.make_invalid (process_errors ~sort:true source errors))
end

