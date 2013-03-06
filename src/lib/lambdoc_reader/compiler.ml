(********************************************************************************)
(*	Compiler.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Compilation of a document Ast.  These functions convert
	a document AST into a proper, final, ambivalent document.
*)

open Lambdoc_core
open Prelude
open Basic
open Ast
open Readconv
open Style

module String = BatString
module List = BatList


(********************************************************************************)
(**	{1 Private data}							*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Exceptions}								*)
(********************************************************************************)

exception Bookmaker_error of Book.isbn_t
exception Mismatched_custom of Custom.kind_t * Custom.kind_t


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type customdef_t =
	| Anonymous
	| Unnumbered of Inline.seq_t
	| Numbered of Inline.seq_t * Order_input.ordinal_counter_t ref


(********************************************************************************)
(**	{2 Auxiliary functions}							*)
(********************************************************************************)

let flatten_map f xs = List.flatten (List.map f xs)

let perhaps f = function
	| [x] -> [f x]
	| _   -> []

let dummy_bookmaker isbns =
	List.map (fun isbn -> (isbn, Bookmaker.Failure Bookmaker.Unavailable)) isbns


(********************************************************************************)
(**	{2 Generic document compilation}					*)
(********************************************************************************)

(**	Compiles an AST as provided by the parser, producing the corresponding
	document.  In addition, a label dictionary, bibliography entries, notes,
	and possible errors are also returned.
*)
let compile_document ?(bookmaker = dummy_bookmaker) ~expand_entities ~idiosyncrasies ast =

	(************************************************************************)
	(* Declaration of some constant values used in the function.		*)
	(************************************************************************)

	(**	Is the usage of macros authorised for this document?  If not,
		we can save time by skipping the coalescing of plain elements.
		We determine whether macros are allowed or not by checking the
		idiosyncrasies of this particular document.
	*)
	let macros_authorised = Idiosyncrasies.check_feature `Feature_macrodef idiosyncrasies in


	(************************************************************************)
	(* Declaration of the local modules used in the function.		*)
	(************************************************************************)

	let module ImageSet = Set.Make (struct type t = Alias.t let compare = String.compare end) in
	let module IsbnSet = Set.Make (struct type t = Book.isbn_t let compare = String.compare end) in

	(************************************************************************)
	(* Declaration of the mutable values used in the function.		*)
	(************************************************************************)

	let pointers = BatDynArray.create ()
	and bibs = BatDynArray.create ()
	and notes = BatDynArray.create ()
	and toc = BatDynArray.create ()
	and images = ref ImageSet.empty
	and isbnrefs = BatDynArray.create ()
	and isbns = ref IsbnSet.empty
        and labels = Hashtbl.create 10
	and customisations = Hashtbl.create 10
	and macros = Hashtbl.create 10
	and errors = BatDynArray.create ()
	and part_counter = Order_input.make_ordinal_counter ()
	and section_counter = Order_input.make_hierarchy_counter ()
	and appendix_counter = Order_input.make_hierarchy_counter ()
	and printout_counter = Order_input.make_ordinal_counter ()
	and equation_counter = Order_input.make_ordinal_counter ()
	and figure_counter = Order_input.make_ordinal_counter ()
	and table_counter = Order_input.make_ordinal_counter ()
	and bib_counter = Order_input.make_ordinal_counter ()
	and note_counter = Order_input.make_ordinal_counter ()
	and custom_counters = Hashtbl.create 10
        and auto_label_counter = ref 0
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
				if Basic_input.matches_ident thing
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
			| Order_input.Invalid_order_format str ->
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
				Order_input.user_hierarchical `Level1 "0"
			| Order_input.Invalid_order_levels (str, expected, found) ->
				let msg = Error.Invalid_order_levels (comm.comm_tag, str, expected, found) in
				BatDynArray.add errors (Some comm.comm_linenum, msg);
				Order_input.user_hierarchical `Level1 "0"


	(*	Adds a new ISBN to the array of referenced books.
	*)
	and add_isbn comm feature isbn =
		BatDynArray.add isbnrefs (comm, feature, isbn);
		isbns := IsbnSet.add isbn !isbns


	(*	Adds a new pointer to the dictionary.
	*)
	and add_pointer target_checker comm pointer =
		if Basic_input.matches_ident pointer
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
		if Idiosyncrasies.check_feature feature idiosyncrasies
		then begin
			Permissions.check ?maybe_minipaged ?maybe_wrapped errors comm feature;
			let (attr, style_parsing) = Style.parse comm errors in
			let element = elem attr style_parsing in
			if Style.dispose comm errors style_parsing
			then Some element
			else None
			
		end else
			let msg = Error.Unavailable_feature (comm.comm_tag, Features.describe feature) in
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


	let rec convert_inline ~depth ~args is_ref inline = match (is_ref, inline) with

		| (_, (comm, Ast.Plain ustr)) ->
			let elem attr _ = [Inline.plain ~attr ustr] in
			check_inline_comm `Feature_plain comm elem

		| (_, (comm, Ast.Entity ent)) ->
			let elem attr _ = match Basic_input.expand_entity ent with
				| `Okay (txt, ustr) -> if expand_entities then [Inline.plain ~attr (ustr :> string)] else [Inline.entity ~attr txt]
				| `Error msg	    -> BatDynArray.add errors (Some comm.comm_linenum, msg); [] in
			check_inline_comm `Feature_entity comm elem

		| (_, (comm, Ast.Linebreak)) ->
			let elem attr _ = [Inline.linebreak ~attr ()] in
			check_inline_comm `Feature_linebreak comm elem

		| (_, (comm, Ast.Mathtex_inl txt)) ->
			let elem attr _ = convert_mathtex (Inline.mathinl ~attr) comm txt in
			check_inline_comm `Feature_mathtex_inl comm elem

		| (_, (comm, Ast.Mathml_inl txt)) ->
			let elem attr _ = convert_mathml (Inline.mathinl ~attr) comm txt in
			check_inline_comm `Feature_mathml_inl comm elem

		| (_, (comm, Ast.Glyph (alias, alt))) ->
			let elem attr _ = images := ImageSet.add alias !images; [Inline.glyph ~attr alias alt] in
			check_inline_comm `Feature_glyph comm elem

		| (x, (comm, Ast.Bold astseq)) ->
			let elem attr _ = [Inline.bold ~attr (convert_seq_aux ~comm ~depth ~args x astseq)] in
			check_inline_comm `Feature_bold comm elem

		| (x, (comm, Ast.Emph astseq)) ->
			let elem attr _ = [Inline.emph ~attr (convert_seq_aux ~comm ~depth ~args x astseq)] in
			check_inline_comm `Feature_emph comm elem

		| (x, (comm, Ast.Code astseq)) ->
			let elem attr _ = [Inline.code ~attr (convert_seq_aux ~comm ~depth ~args x astseq)] in
			check_inline_comm `Feature_code comm elem

		| (x, (comm, Ast.Caps astseq)) ->
			let elem attr _ = [Inline.caps ~attr (convert_seq_aux ~comm ~depth ~args x astseq)] in
			check_inline_comm `Feature_caps comm elem

		| (x, (comm, Ast.Ins astseq)) ->
			let elem attr _ = [Inline.ins ~attr (convert_seq_aux ~comm ~depth ~args x astseq)] in
			check_inline_comm `Feature_ins comm elem

		| (x, (comm, Ast.Del astseq)) ->
			let elem attr _ = [Inline.del ~attr (convert_seq_aux ~comm ~depth ~args x astseq)] in
			check_inline_comm `Feature_del comm elem

		| (x, (comm, Ast.Sup astseq)) ->
			let elem attr _ = [Inline.sup ~attr (convert_seq_aux ~comm ~depth ~args x astseq)] in
			check_inline_comm `Feature_sup comm elem

		| (x, (comm, Ast.Sub astseq)) ->
			let elem attr _ = [Inline.sub ~attr (convert_seq_aux ~comm ~depth ~args x astseq)] in
			check_inline_comm `Feature_sub comm elem

		| (x, (comm, Ast.Mbox astseq)) ->
			let elem attr _ = [Inline.mbox ~attr (convert_seq_aux ~comm ~depth ~args x astseq)] in
			check_inline_comm `Feature_mbox comm elem

		| (x, (comm, Ast.Span astseq)) ->
			let elem attr _ = [Inline.span ~attr (convert_seq_aux ~comm ~depth ~args x astseq)] in
			check_inline_comm `Feature_span comm elem

		| (false, (comm, Ast.Link (uri, maybe_astseq))) ->
			let elem attr _ =
				let maybe_seq = maybe (convert_seq_aux ~comm ~depth ~args true) maybe_astseq in
				[Inline.link ~attr uri maybe_seq] in
			check_inline_comm `Feature_link comm elem

		| (false, (comm, Ast.Booklink (isbn, maybe_astseq))) ->
			let elem attr dict =
				let rating = Style.consume1 dict (Rating_hnd, None) in
				let maybe_seq = maybe (convert_seq_aux ~comm ~depth ~args true) maybe_astseq in
				add_isbn comm `Feature_booklink isbn;
				[Inline.booklink ~attr isbn rating maybe_seq] in
			check_inline_comm `Feature_booklink comm elem

		| (false, (comm, Ast.See refs)) ->
			let elem attr _ =
				let target_checker = function
					| Target.Note_target _	-> `Valid_target
					| _			-> `Wrong_target Error.Target_note in
				List.iter (add_pointer target_checker comm) refs;
				match refs with
					| hd::tl ->
						[Inline.see ~attr (hd, tl)]
					| [] ->
						let msg = Error.Empty_list comm.comm_tag in
						BatDynArray.add errors (Some comm.comm_linenum, msg);
						[] in
			check_inline_comm `Feature_see comm elem

		| (false, (comm, Ast.Cite refs)) ->
			let elem attr _ =
				let target_checker = function
					| Target.Bib_target _	-> `Valid_target
					| _			-> `Wrong_target Error.Target_bib in
				List.iter (add_pointer target_checker comm) refs;
				match refs with
					| hd::tl ->
						[Inline.cite ~attr (hd, tl)]
					| [] ->
						let msg = Error.Empty_list comm.comm_tag in
						BatDynArray.add errors (Some comm.comm_linenum, msg);
						[] in
			check_inline_comm `Feature_cite comm elem

		| (false, (comm, Ast.Ref (pointer, maybe_astseq))) ->
			let elem attr _ =
				let target_checker target = match (maybe_astseq, target) with
					| (None, Target.Visible_target (Target.Custom_target (_, _, `None_given)))
					| (None, Target.Visible_target (Target.Wrapper_target (_, `None_given)))
					| (None, Target.Visible_target (Target.Part_target `None_given))
					| (None, Target.Visible_target (Target.Section_target (_, `None_given))) -> `Empty_target
					| (_, Target.Visible_target _)						 -> `Valid_target
					| _									 -> `Wrong_target Error.Target_label in
				add_pointer target_checker comm pointer;
				let maybe_seq = maybe (convert_seq_aux ~comm ~depth ~args true) maybe_astseq in
				[Inline.ref ~attr pointer maybe_seq] in
			check_inline_comm `Feature_ref comm elem

		| (false, (comm, Ast.Sref pointer)) ->
			let elem attr _ =
				let target_checker = function
					| Target.Visible_target (Target.Custom_target (_, _, `None_given))
					| Target.Visible_target (Target.Wrapper_target (_, `None_given))
					| Target.Visible_target (Target.Part_target `None_given)
					| Target.Visible_target (Target.Section_target (_, `None_given)) -> `Empty_target
					| Target.Visible_target _					 -> `Valid_target
					| _								 -> `Wrong_target Error.Target_label in
				add_pointer target_checker comm pointer;
				[Inline.sref ~attr pointer] in
			check_inline_comm `Feature_sref comm elem

		| (_, (comm, Ast.Macroarg raw_num)) ->
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

		| (x, (comm, Ast.Macrocall (name, arglist))) ->
			let elem attr _ =
				try
					let (macro_nargs, macro_astseq) = Hashtbl.find macros name in
					if macro_nargs <> List.length arglist
					then
						let msg = Error.Invalid_macro_call (name, List.length arglist, macro_nargs) in
						BatDynArray.add errors (Some comm.comm_linenum, msg);
						[]
					else
						let new_arglist = List.map (convert_inline_list ~comm ~depth:(depth+1) ~args x) arglist in
						convert_inline_list ~comm ~depth:(depth+1) ~args:(Some new_arglist) x macro_astseq
				with
					| Not_found ->
						let msg = Error.Undefined_macro (comm.comm_tag, name) in
						BatDynArray.add errors (Some comm.comm_linenum, msg);
						[] in
			check_inline_comm `Feature_macrocall comm elem

		| (_, (comm, _)) ->
			let msg = Error.Unexpected_inline comm.comm_tag in
			BatDynArray.add errors (Some comm.comm_linenum, msg);
			[]

	and convert_inline_list ~comm ~depth ~args is_ref astseq =
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
		let seq = flatten_map (convert_inline ~depth ~args is_ref) astseq in
		let new_seq = if macros_authorised || expand_entities then coalesce_plain seq else seq in
		match new_seq with
			| [] ->
				let msg = Error.Empty_sequence comm.comm_tag in
				BatDynArray.add errors (Some comm.comm_linenum, msg);
				[dummy_inline]
			| xs ->
				xs

	and convert_seq_aux ~comm ~depth ~args is_ref astseq =
		let seq = convert_inline_list ~comm ~depth ~args is_ref astseq in
		(List.hd seq, List.tl seq) in

	let convert_seq ~comm ?args seq =
		convert_seq_aux ~comm ~depth:0 ?args false seq in


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
				| []		-> invalid_arg "Parser has given us an empty tabular row"
				| hd::tl	-> Tabular.make_row (nemap converter (hd, tl)) in
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
				| []		-> invalid_arg "Parser has given us an empty tabular group"
				| hd::tl	-> Tabular.make_group (nemap convert_row (hd, tl)) in

		let thead = maybe (convert_group `Feature_thead) tab.thead in

		let tfoot = maybe (convert_group `Feature_tfoot) tab.tfoot in

		match tab.tbodies with
			| []		-> invalid_arg "Parser has given us an empty tabular body"
			| hd::tl	-> Tabular.make specs ?thead ?tfoot (nemap (convert_group `Feature_tbody) (hd, tl)) in


	(************************************************************************)
	(* Compilation functions for document blocks.				*)
	(************************************************************************)

	let dummy_block = Block.paragraph (dummy_inline, []) in


	let check_block_comm ?maybe_minipaged ?maybe_wrapped feature comm elem =
		match check_comm ?maybe_minipaged ?maybe_wrapped feature comm elem with
			| Some x -> x
			| None	 -> [dummy_block] in


	let rec convert_block ~minipaged allow_above_listable allow_above_quotable allow_above_embeddable allowed_blk block =
		match (allow_above_listable, allow_above_quotable, allow_above_embeddable, allowed_blk, block) with

		| (_, _, _, `Paragraph_blk, (comm, Ast.Paragraph astseq))
		| (_, _, _, `Any_blk, (comm, Ast.Paragraph astseq)) ->
			let elem attr _ = [Block.paragraph ~attr (convert_seq ~comm astseq)] in
			check_block_comm `Feature_paragraph comm elem

		| (_, x1, x2, `Any_blk, (comm, Ast.Itemize astfrags)) ->
			let elem attr _ = convert_frag_of_anon_frags ~comm ~cons:(Block.itemize ~attr) ~minipaged x1 x2 astfrags in
			check_block_comm `Feature_itemize comm elem

		| (_, x1, x2, `Any_blk, (comm, Ast.Enumerate astfrags)) ->
			let elem attr _ = convert_frag_of_anon_frags ~comm ~cons:(Block.enumerate ~attr) ~minipaged x1 x2 astfrags in
			check_block_comm `Feature_enumerate comm elem

		| (_, x1, x2, `Any_blk, (comm, Ast.Description astfrags)) ->
			let elem attr _ = convert_frag_of_desc_frags ~comm ~cons:(Block.description ~attr) ~minipaged x1 x2 astfrags in
			check_block_comm `Feature_description comm elem

		| (_, x1, x2, `Any_blk, (comm, Ast.Qanda astfrags)) ->
			let elem attr _ = convert_frag_of_qanda_frags ~comm ~cons:(Block.qanda ~attr) ~minipaged x1 x2 astfrags in
			check_block_comm `Feature_qanda comm elem

		| (_, _, _, `Any_blk, (comm, Ast.Verse astfrag)) ->
			let elem attr _ = [Block.verse ~attr (convert_frag_aux ~comm ~minipaged false false false `Paragraph_blk astfrag)] in
			check_block_comm `Feature_verse comm elem

		| (_, _, true, `Any_blk, (comm, Ast.Quote astfrag)) ->
			let elem attr _ = [Block.quote ~attr (convert_frag_aux ~comm ~minipaged false false true `Any_blk astfrag)] in
			check_block_comm `Feature_quote comm elem

		| (_, _, _, `Equation_blk, (comm, Ast.Mathtex_blk txt))
		| (_, _, _, `Any_blk, (comm, Ast.Mathtex_blk txt)) ->
			let elem attr _ = convert_mathtex (Block.mathblk ~attr) comm txt in
			check_block_comm `Feature_mathtex_blk comm elem

		| (_, _, _, `Equation_blk, (comm, Ast.Mathml_blk txt))
		| (_, _, _, `Any_blk, (comm, Ast.Mathml_blk txt)) ->
			let elem attr _ = convert_mathml (Block.mathblk ~attr) comm txt in
			check_block_comm `Feature_mathml_blk comm elem

		| (_, _, _, `Printout_blk, (comm, Ast.Source txt))
		| (_, _, _, `Any_blk, (comm, Ast.Source txt)) ->
			let elem attr dict =
				let (lang, linenums) = Style.consume2 dict (Lang_hnd, None) (Linenums_hnd, false) in
				let trimmed = Literal_input.trim txt in
				let hilite = Camlhighlight_parser.from_string ?lang trimmed in
				let src = Source.make lang hilite linenums in
				let () = if trimmed = "" then BatDynArray.add errors (Some comm.comm_linenum, Error.Empty_source comm.comm_tag) in
				[Block.source ~attr src] in
			check_block_comm `Feature_source comm elem

		| (_, _, _, `Table_blk, (comm, Ast.Tabular (tcols, tab)))
		| (_, _, _, `Any_blk, (comm, Ast.Tabular (tcols, tab))) ->
			let elem attr _ = [Block.tabular ~attr (convert_tabular comm tcols tab)] in
			check_block_comm `Feature_tabular comm elem

		| (_, true, true, `Figure_blk, (comm, Ast.Subpage astfrag))
		| (_, true, true, `Any_blk, (comm, Ast.Subpage astfrag)) ->
			let elem attr _ = [Block.subpage ~attr (convert_frag_aux ~comm ~minipaged:true true true true `Any_blk astfrag)] in
			check_block_comm `Feature_subpage comm elem

		| (_, _, _, `Figure_blk, (comm, Ast.Verbatim txt))
		| (_, _, _, `Any_blk, (comm, Ast.Verbatim txt)) ->
			let elem attr _ =
				let trimmed = Literal_input.trim txt in
				let () = if trimmed = "" then BatDynArray.add errors (Some comm.comm_linenum, Error.Empty_verbatim comm.comm_tag) in
				[Block.verbatim ~attr trimmed] in
			check_block_comm `Feature_verbatim comm elem

		| (_, _, _, `Figure_blk, (comm, Ast.Picture (alias, alt)))
		| (_, _, _, `Any_blk, (comm, Ast.Picture (alias, alt))) ->
			let elem attr dict =
				let width = Style.consume1 dict (Width_hnd, None) in
				images := ImageSet.add alias !images;
				[Block.picture ~attr width alias alt] in
			check_block_comm `Feature_picture comm elem

		| (_, _, _, `Any_blk, (comm, Ast.Bookpic isbn)) ->
			let elem attr dict =
				let (rating, coversize) = Style.consume2 dict (Rating_hnd, None) (Coversize_hnd, Book.Medium) in
				add_isbn comm `Feature_bookpic isbn;
				[Block.bookpic ~attr isbn rating coversize] in
			check_block_comm `Feature_bookpic comm elem

		| (_, true, true, `Any_blk, (comm, Ast.Pullquote (maybe_astseq, astfrag))) ->
			let elem attr _ =
				let maybe_seq = maybe (convert_seq ~comm) maybe_astseq in
				let frag = convert_frag_aux ~comm ~minipaged false false false `Any_blk astfrag in
				[Block.pullquote ~attr maybe_seq frag] in
			check_block_comm `Feature_pullquote comm elem

		| (_, true, true, `Any_blk, (comm, Ast.Custom (maybe_kind, env, maybe_astseq, astfrag))) ->
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
					let (block_maker, allow_above_embeddable) = match kind with
						| Custom.Boxout  -> (Block.boxout ~attr (Custom.Boxout.make data), true)
						| Custom.Theorem -> (Block.theorem ~attr (Custom.Theorem.make data), false) in
					let maybe_seq = maybe (convert_seq ~comm) maybe_astseq in
					let frag = convert_frag_aux ~comm ~minipaged false false allow_above_embeddable `Any_blk astfrag in
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

		| (_, true, true, `Any_blk, (comm, Ast.Equation (maybe_astseq, astblk))) ->
			let elem attr _ =
				let wrapper = convert_wrapper comm equation_counter Wrapper.Equation maybe_astseq in
				let blk = convert_block ~minipaged true true true `Equation_blk astblk in
				perhaps (Block.equation ~attr wrapper) blk in
			check_block_comm ~maybe_minipaged:(Some minipaged) `Feature_equation comm elem

		| (_, true, true, `Any_blk, (comm, Ast.Printout (maybe_astseq, astblk))) ->
			let elem attr _ =
				let wrapper = convert_wrapper comm printout_counter Wrapper.Printout maybe_astseq in
				let blk = convert_block ~minipaged true true true `Printout_blk astblk in
				perhaps (Block.printout ~attr wrapper) blk in
			check_block_comm ~maybe_minipaged:(Some minipaged) `Feature_printout comm elem

		| (_, true, true, `Any_blk, (comm, Ast.Table (maybe_astseq, astblk))) ->
			let elem attr _ =
				let wrapper = convert_wrapper comm table_counter Wrapper.Table maybe_astseq in
				let blk = convert_block ~minipaged true true true `Table_blk astblk in
				perhaps (Block.table ~attr wrapper) blk in
			check_block_comm ~maybe_minipaged:(Some minipaged) `Feature_table comm elem

		| (_, true, true, `Any_blk, (comm, Ast.Figure (maybe_astseq, astblk))) ->
			let elem attr _ =
				let wrapper = convert_wrapper comm figure_counter Wrapper.Figure maybe_astseq in
				let blk = convert_block ~minipaged true true true `Figure_blk astblk in
				perhaps (Block.figure ~attr wrapper) blk in
			check_block_comm ~maybe_minipaged:(Some minipaged) `Feature_figure comm elem

		| (true, true, true, `Any_blk, (comm, Ast.Part astseq)) ->
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

		| (true, true, true, `Any_blk, (comm, Ast.Appendix)) ->
			let elem attr _ =
				let order = Order_input.no_order () in
				let label = make_label comm (Target.part order) in
				let heading = Heading.appendix label in
				let block = Block.heading ~attr heading in
				let () = if not minipaged then add_toc_entry heading in
				let () = appendixed := true in
				[block] in
			check_block_comm ~maybe_minipaged:(Some minipaged) `Feature_appendix comm elem

		| (true, true, true, `Any_blk, (comm, Ast.Section (level, astseq))) ->
			let elem attr _ =
				let (counter, location) =
					if !appendixed
					then (appendix_counter, Heading.Appendixed)
					else (section_counter, Heading.Mainbody) in
				let order = match comm.comm_order with
					| None	     -> Order_input.auto_hierarchical level counter
					| Some ""    -> Order_input.no_order ()
					| Some other -> make_user_hierarchical comm level other in
				let label = make_label comm (Target.section location order) in
				let heading = Heading.section label order location level (convert_seq ~comm astseq) in
				let block = Block.heading ~attr heading in
				let () = if not minipaged then add_toc_entry heading in
				[block] in
			let feature = match level with
				| `Level1 -> `Feature_section1
				| `Level2 -> `Feature_section2
				| `Level3 -> `Feature_section3 in
			check_block_comm ~maybe_minipaged:(Some minipaged) feature comm elem

		| (true, true, true, `Any_blk, (comm, Ast.Bibliography)) ->
			convert_preset_sectional ~tocable:true ~minipaged Heading.bibliography `Feature_bibliography comm

		| (true, true, true, `Any_blk, (comm, Ast.Notes)) ->
			convert_preset_sectional ~tocable:true ~minipaged Heading.notes `Feature_notes comm 

		| (true, true, true, `Any_blk, (comm, Ast.Toc)) ->
			convert_preset_sectional ~tocable:false ~minipaged Heading.toc `Feature_toc comm

		| (true, true, true, `Any_blk, (comm, Ast.Title (level, astseq))) ->
			let elem attr _ = [Block.title ~attr level (convert_seq ~comm astseq)] in
			let feature = match level with
				| `Level1 -> `Feature_title1
				| `Level2 -> `Feature_title2 in
			check_block_comm feature comm elem

		| (true, true, true, `Any_blk, (comm, Ast.Abstract astfrag)) ->
			let elem attr _ =
				let frag = convert_frag_aux ~comm ~minipaged false false false `Any_blk astfrag in
				[Block.abstract ~attr frag] in
			check_block_comm `Feature_abstract comm elem

		| (true, true, true, `Any_blk, (comm, Ast.Rule)) ->
			let elem attr _ = [Block.rule ~attr ()] in
			check_block_comm `Feature_rule comm elem

		| (_, _, _, `Any_blk, (comm, Ast.Bib bib)) ->
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

		| (_, _, _, `Any_blk, (comm, Ast.Note astfrag)) ->
			let elem attr _ =
				let order = Order_input.auto_ordinal note_counter in
				let label = make_label comm (Target.note order) in
				let frag = convert_frag_aux ~comm ~minipaged:true false true true `Any_blk astfrag in
				let note = Note.make label order frag in
				BatDynArray.add notes note;
				[] in
			check_block_comm `Feature_note comm elem

		| (_, _, _, `Any_blk, (comm, Ast.Macrodef (name, nargs, astseq))) ->
			let elem attr _ =
				if not (Basic_input.matches_ident name)
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

		| (_, _, _, `Any_blk, (comm, Ast.Boxoutdef (env, maybe_caption, maybe_counter_name))) ->
			let elem attr _ = convert_customdef comm env Custom.Boxout maybe_caption maybe_counter_name; [] in
			check_block_comm `Feature_boxoutdef comm elem

		| (_, _, _, `Any_blk, (comm, Ast.Theoremdef (env, caption, maybe_counter_name))) ->
			let elem attr _ = convert_customdef comm env Custom.Theorem (Some caption) maybe_counter_name; [] in
			check_block_comm `Feature_theoremdef comm elem

		| (_, _, _, _, (comm, _)) ->
			let blk = match allowed_blk with
				| `Paragraph_blk
				| `Equation_blk
				| `Printout_blk
				| `Table_blk
				| `Figure_blk as blk -> blk
				| `Any_blk -> match (allow_above_listable, allow_above_quotable, allow_above_embeddable) with
					| (_, _, false) -> `Embeddable_blk
					| (_, false, _) -> `Quotable_blk
					| (false, _, _) -> `Listable_blk
					| _		-> `Super_blk in
			let msg = Error.Unexpected_block (comm.comm_tag, blk) in
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
				Wrapper.Unordered (label, (dummy_inline, []))
			| (`None_given, Some seq) ->
				Wrapper.Unordered (label, seq)
			| (`Auto_given _ as o, maybe_seq)
			| (`User_given _ as o, maybe_seq) ->
				Wrapper.Ordered (label, o, maybe_seq)


	and convert_customdef comm env kind maybe_caption maybe_counter_name =
		if not (Basic_input.matches_ident env)
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
				if Basic_input.matches_ident counter_name
				then begin
					let counter = Order_input.make_ordinal_counter () in
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


	and convert_frag_of_anon_frags ~comm ~cons ~minipaged allow_above_quotable allow_above_embeddable astfrags =
		let conv (comm, astfrag) =
			let elem attr _ = convert_frag_aux ~comm ~minipaged false allow_above_quotable allow_above_embeddable `Any_blk astfrag in
			check_comm `Feature_item comm elem in
		let frags = List.filter_map conv astfrags in
		match frags with
			| [] ->
				let msg = Error.Empty_fragment comm.comm_tag in
				BatDynArray.add errors (Some comm.comm_linenum, msg);
				[dummy_block]
			| hd :: tl ->
				[cons (hd, tl)]


	and convert_frag_of_desc_frags ~comm ~cons ~minipaged allow_above_quotable allow_above_embeddable astfrags =
		let conv (comm, astseq, astfrag) =
			let elem attr _ =
				let seq = convert_seq ~comm astseq in
				let frag = convert_frag_aux ~comm ~minipaged false allow_above_quotable allow_above_embeddable `Any_blk astfrag in
				(seq, frag) in
			check_comm `Feature_item comm elem in
		let frags = List.filter_map conv astfrags in
		match frags with
			| [] ->
				let msg = Error.Empty_fragment comm.comm_tag in
				BatDynArray.add errors (Some comm.comm_linenum, msg);
				[dummy_block]
			| hd :: tl ->
				[cons (hd, tl)]


	and convert_frag_of_qanda_frags ~comm ~cons ~minipaged allow_above_quotable allow_above_embeddable astfrags =
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
				let frag = convert_frag_aux ~comm ~minipaged false allow_above_quotable allow_above_embeddable `Any_blk astfrag in
				[(qanda, frag)] in
			check_comm feature comm elem in
		let frags = List.flatten (List.filter_map conv astfrags) in
		match frags with
			| [] ->
				let msg = Error.Empty_fragment comm.comm_tag in
				BatDynArray.add errors (Some comm.comm_linenum, msg);
				[dummy_block]
			| hd :: tl ->
				[cons (hd, tl)]


	and convert_frag_aux ?comm ~minipaged allow_above_listable allow_above_quotable allow_above_embeddable allowed_blk astfrag =
		let conv astblk = convert_block ~minipaged allow_above_listable allow_above_quotable allow_above_embeddable allowed_blk astblk in
		let frag = flatten_map conv astfrag in
		match frag with
			| [] ->
				let (tag, linenum) = match comm with
					| Some comm -> (comm.comm_tag, Some comm.comm_linenum)
					| None	    -> (None, None) in
				let msg = Error.Empty_fragment tag in
				BatDynArray.add errors (linenum, msg);
				(dummy_block, [])
			| hd :: tl ->
				(hd, tl) in


	let convert_frag astfrag =
		convert_frag_aux ~minipaged:false true true true `Any_blk astfrag in


	(************************************************************************)
	(* Filtering of pointers.						*)
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
	(* Retrieve book data for all referenced ISBNs.				*)
	(************************************************************************)

	let retrieve_books () =
		let books = Hashtbl.create 0 in
		if not (IsbnSet.is_empty !isbns)
		then begin
			let open Bookmaker in
			let results = bookmaker (IsbnSet.elements !isbns) in
			let process_isbnref (comm, feature, isbn) =
				try match List.assoc isbn results with
					| Success book ->
						Hashtbl.add books isbn book
					| Failure failure ->
						let msg = match failure with
							| Unavailable	   -> Error.Unavailable_bookmaker (comm.comm_tag, Features.describe feature)
							| Uncapable err    -> Error.Uncapable_bookmaker (comm.comm_tag, Features.describe feature, err)
							| Malformed_ISBN _ -> Error.Malformed_ISBN (comm.comm_tag, isbn)
							| Unknown_ISBN _   -> Error.Unknown_ISBN (comm.comm_tag, isbn) in
						BatDynArray.add errors (Some comm.comm_linenum, msg)
				with
					Not_found -> raise (Bookmaker_error isbn) in
			BatDynArray.iter process_isbnref isbnrefs
		end;
		books in


	(************************************************************************)
	(* Wrap-up.								*)
	(************************************************************************)

	let contents = convert_frag ast in
	let custom = filter_customisations () in
	let () = filter_pointers () in
	let books = retrieve_books () in
	(contents, BatDynArray.to_list bibs, BatDynArray.to_list notes, BatDynArray.to_list toc, ImageSet.elements !images, books, labels, custom, BatDynArray.to_list errors)


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
		let format_error (error_linenum, error_msg) =
			let error_context = match error_linenum with
				| Some num ->
					Some	{
						Error.error_line_number = num;
						Error.error_line_before = if num >= 2 then [source_lines.(num - 2)] else [];
						Error.error_line_actual = source_lines.(num - 1);
						Error.error_line_after = if num < (Array.length source_lines) then [source_lines.(num)] else []
						}
				| None ->
					None in
			(error_context, error_msg) in
		List.map format_error errors


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

(**	Process and (optionally) sort the errors by line number.
*)
let process_errors ~sort source errors =
	let compare (a, _) (b, _) =
		let ord_a = match a with Some x -> x.Error.error_line_number | None -> 0
		and ord_b = match b with Some x -> x.Error.error_line_number | None -> 0 in
		Pervasives.compare ord_a ord_b in
	let collated = collate_errors source errors in
	let sorted = if sort then List.stable_sort compare collated else collated in
	match sorted with
		| []	   -> failwith "Compiler.process_errors"
		| hd :: tl -> (hd, tl)


(**	Compile a document AST into a manuscript.
*)
let compile ?bookmaker ~expand_entities ~feature_ruleset ~feature_default ~classname_ruleset ~classname_default ~source ast =
	let idiosyncrasies = Idiosyncrasies.make ~feature_ruleset ~feature_default ~classname_ruleset ~classname_default in
	let (contents, bibs, notes, toc, images, books, labels, custom, errors) = compile_document ?bookmaker ~expand_entities ~idiosyncrasies ast in
	match errors with
		| [] -> Ambivalent.make_valid contents bibs notes toc images books labels custom
		| xs -> Ambivalent.make_invalid (process_errors ~sort:true source errors)

