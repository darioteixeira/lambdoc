(********************************************************************************)
(*  Lambdoc_reader_compiler.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Compilation of a document Ast.  These functions convert
    a document AST into a proper, final, ambivalent document.
*)

module Ast = Lambdoc_reader_ast
module Extension = Lambdoc_reader_extension
module Permission = Lambdoc_reader_permission
module Readconv = Lambdoc_reader_readconv
module Style = Lambdoc_reader_style

open Lambdoc_prelude
open Lambdoc_core
open Ast
open Readconv
open Style
open Idiosyncrasies


(********************************************************************************)
(** {1 Private type definitions}                                                *)
(********************************************************************************)

type customdef =
    | Anonymous
    | Unnumbered of Inline.seq
    | Numbered of Inline.seq * Order_input.ordinal_counter ref


(********************************************************************************)
(** {1 Functors}                                                                *)
(********************************************************************************)

module Make (Ext: Extension.S) =
struct

open Ext
open Foldmapper


(********************************************************************************)
(** {1 Exceptions}                                                              *)
(********************************************************************************)

exception Internal_extension_error of Ast.command


(********************************************************************************)
(** {2 Auxiliary functions and operators for monad}                             *)
(********************************************************************************)

let (>>=) = Monad.bind


let (>|=) m f =
    m >>= fun res ->
    Monad.return (f res)


let monadic_maybe f = function
    | Some x ->
        f x >>= fun x' ->
        Monad.return (Some x')
    | None ->
        Monad.return None


let rec monadic_map f = function
    | [] ->
        Monad.return []
    | hd :: tl ->
        f hd >>= fun hd' ->
        monadic_map f tl >>= fun tl' ->
        Monad.return (hd' :: tl')


let rec monadic_filter_map f = function
    | [] ->
        Monad.return []
    | hd :: tl ->
        f hd >>= function
            | Some hd' ->
                monadic_filter_map f tl >>= fun tl' ->
                Monad.return (hd' :: tl')
            | None ->
                monadic_filter_map f tl


(********************************************************************************)
(** {2 Other miscelaneous auxiliary functions}                                  *)
(********************************************************************************)

let rec all_equal = function
    | []           -> true
    | [_]          -> true
    | x :: y :: tl -> x = y && all_equal (y :: tl)


(********************************************************************************)
(** {2 Public functions and values}                                             *)
(********************************************************************************)

let contextualize_errors ~sort source errors =
    let source_lines = String.asplit source in
    let num_lines = Array.length source_lines in
    let format_error (error_linenum, error_tag, error_msg) =
        let error_context = match error_linenum with
            | Some num ->
                let num = max 1 (min num num_lines) in
                Some
                    {
                    Error.error_line_number = num;
                    Error.error_line_before = if num >= 2 then [source_lines.(num - 2)] else [];
                    Error.error_line_actual = source_lines.(num - 1);
                    Error.error_line_after = if num < num_lines then [source_lines.(num)] else []
                    }
            | None ->
                None in
        (error_context, error_tag, error_msg) in
    let compare (anum, _, amsg) (bnum, _, bmsg) = match (anum, bnum) with
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
    let sorted = if sort then List.sort_uniq compare errors else errors in
    List.map format_error sorted


(** Compiles an AST as provided by the parser, producing the corresponding document.
*)
let compile ?postprocessor ~extcomms ~expand_entities ~idiosyncrasies ~source ast =

    (****************************************************************************)
    (* Declaration of some constant values used in the function.                *)
    (****************************************************************************)

    (** Is the usage of macros authorised for this document?  If not,
        we can save time by skipping the coalescing of plain elements.
        We determine whether macros are allowed or not by checking the
        idiosyncrasies of this particular document.
    *)
    let macros_authorised = Permission.check_feature `Feature_macrodef idiosyncrasies in


    (****************************************************************************)
    (* Declaration of the mutable values used in the function.                  *)
    (****************************************************************************)

    let pointers = ref [] in
    let bibs = ref [] in
    let notes = ref [] in
    let toc = ref [] in
    let labels = Hashtbl.create 10 in
    let customisations = Hashtbl.create 10 in
    let macros = Hashtbl.create 10 in
    let errors = ref [] in
    let part_counter = Order_input.ordinal_counter () in
    let section_counter = Order_input.hierarchical_counter () in
    let appendix_counter = Order_input.hierarchical_counter () in
    let printout_counter = Order_input.ordinal_counter () in
    let equation_counter = Order_input.ordinal_counter () in
    let figure_counter = Order_input.ordinal_counter () in
    let table_counter = Order_input.ordinal_counter () in
    let bib_counter = Order_input.ordinal_counter () in
    let note_counter = Order_input.ordinal_counter () in
    let custom_counters = Hashtbl.create 10 in
    let auto_label_counter = ref 0 in
    let appendixed = ref false in
    let has_bibliography_refs = ref false in
    let has_bibliography_section = ref false in
    let has_notes_refs = ref false in
    let has_notes_section = ref false in


    (****************************************************************************)
    (* Split the extcomms into inline and block variants.                       *)
    (****************************************************************************)

    let (inline_extcomms, block_extcomms) =
        let unzip_extcomm (accum_inl, accum_blk) (tag, def) = match def with
            | Inlextcomm inlfun            -> ((tag, inlfun) :: accum_inl, accum_blk)
            | Blkextcomm (blkfun, blkcats) -> (accum_inl, (tag, (blkfun, blkcats)) :: accum_blk)
        in List.fold_left unzip_extcomm ([], []) extcomms in


    (****************************************************************************)
    (* Dummy elements.                                                          *)
    (****************************************************************************)

    let dummy_inline = Inline.linebreak () in
    let dummy_cell = Tabular.make_cell None in
    let dummy_block = Block.paragraph [dummy_inline] in


    (****************************************************************************)
    (* Helper sub-functions.                                                    *)
    (****************************************************************************)

    (*  Add an error message to the list of errors.
    *)
    let add_error comm msg =
        errors := (Some comm.comm_linenum, comm.comm_tag, msg) :: !errors in


    (*  This subfunction creates a new label.  It checks whether the user
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
                    then
                        let msg = Error.Duplicate_target thing in
                        add_error comm msg
                    else
                        Hashtbl.add labels new_label target
                else
                    if thing <> ""
                    then
                        let msg = Error.Invalid_label thing in
                        add_error comm msg
            end;
            new_label
        | None ->
            incr auto_label_counter;
            Label.Auto (string_of_int !auto_label_counter)


    (*  This subfunction creates an user ordinal order, checking
        for exceptions and appending the error list if necessary.
    *)
    and make_user_ordinal comm str =
        try
            Order_input.user_ordinal str
        with
            Order_input.Invalid_order_format str ->
                let msg = Error.Invalid_order_format str in
                add_error comm msg;
                Order_input.user_ordinal "0"


    (*  This subfunction creates an user hierarchical order, checking
        for exceptions and appending the error list if necessary.
    *)
    and make_user_hierarchical comm level str =
        try
            Order_input.user_hierarchical level str
        with
            | Order_input.Invalid_order_format str ->
                let msg = Error.Invalid_order_format str in
                add_error comm msg;
                Order_input.user_hierarchical (Level.section 1) "0"
            | Order_input.Invalid_order_levels (str, expected, found) ->
                let msg = Error.Invalid_order_levels (str, expected, found) in
                add_error comm msg;
                Order_input.user_hierarchical (Level.section 1) "0"


    (*  Adds a new pointer to the dictionary.
    *)
    and add_pointer target_checker comm pointer =
        if Identifier_input.matches_label pointer
        then
            pointers := (target_checker, comm, pointer) :: !pointers
        else
            let msg = Error.Invalid_label pointer in
            add_error comm msg


    (*  Adds a new TOC entry.
    *)
    and add_toc_entry heading =
        toc := heading :: !toc


    (*  Checker for inline/block commands.
    *)
    and check_comm ?maybe_minipaged ?maybe_wrapped feature comm elem =
        let check_classname classname =
            if not (Permission.check_classname feature classname idiosyncrasies)
            then 
                let msg = Error.Invalid_style_misplaced_classname classname in
                add_error comm msg in
        if comm.comm_originator = Extension || Permission.check_feature feature idiosyncrasies
        then begin
            let permission_error_msgs = Permission.check_parameters ?maybe_minipaged ?maybe_wrapped comm feature in
            List.iter (add_error comm) permission_error_msgs;
            let (classnames, style_parsing, style_error_msgs) = Style.parse comm in
            List.iter (add_error comm) style_error_msgs;
            List.iter check_classname classnames;
            let parsinfo = Attr.{tag = comm.comm_tag; linenum = comm.comm_linenum; originator = comm.comm_originator} in
            let attr = Attr.make ~parsinfo classnames in
            elem attr style_parsing >>= fun element ->
            let dispose_error_msgs = Style.dispose comm style_parsing in
            List.iter (add_error comm) dispose_error_msgs;
            if dispose_error_msgs = []
            then Monad.return (Some element)
            else Monad.return None
        end else
            let msg = Error.Unavailable_feature (Feature.describe feature) in
            add_error comm msg;
            Monad.return None in


    let check_inline_comm ?maybe_minipaged ?maybe_wrapped feature comm elem =
        check_comm ?maybe_minipaged ?maybe_wrapped feature comm elem >>= function
            | Some x -> Monad.return x
            | None   -> Monad.return [dummy_inline] in


    let check_block_comm ?maybe_minipaged ?maybe_wrapped feature comm elem =
        check_comm ?maybe_minipaged ?maybe_wrapped feature comm elem >>= function
            | Some x -> Monad.return x
            | None   -> Monad.return [dummy_block] in


    (****************************************************************************)
    (* Compilation functions for mathematics.                                   *)
    (****************************************************************************)

    let convert_mathtex constructor comm mathtex =
        try
            [constructor (Math_input.from_mathtex mathtex)]
        with _ ->
            let msg = Error.Invalid_mathtex mathtex in
            add_error comm msg;
            []


    and convert_mathml constructor comm mathml =
        try
            [constructor (Math_input.from_mathml mathml)]
        with _ ->
            let msg = Error.Invalid_mathml mathml in
            add_error comm msg;
            [] in


    (****************************************************************************)
    (* Compilation functions for inline context.                                *)
    (****************************************************************************)

    let rec convert_inline ~context ~depth ~args is_ref (comm, inline) = match inline with

        | Ast.Plain ustr ->
            let elem attr _ = Monad.return [Inline.plain ~attr ustr] in
            check_inline_comm `Feature_plain comm elem

        | Ast.Entity ent ->
            let elem attr _ = match Entity_input.expand ent with
                | `Okay (txt, ustr) ->
                    if expand_entities
                    then Monad.return [Inline.plain ~attr (ustr :> string)]
                    else Monad.return [Inline.entity ~attr txt]
                | `Error msg ->
                    add_error comm msg;
                    Monad.return [] in
            check_inline_comm `Feature_entity comm elem

        | Ast.Linebreak ->
            let elem attr _ = Monad.return [Inline.linebreak ~attr ()] in
            check_inline_comm `Feature_linebreak comm elem

        | Ast.Mathtex_inl txt ->
            let elem attr _ = Monad.return (convert_mathtex (Inline.math_inl ~attr) comm txt) in
            check_inline_comm `Feature_mathtex_inl comm elem

        | Ast.Mathml_inl txt ->
            let elem attr _ = Monad.return (convert_mathml (Inline.math_inl ~attr) comm txt) in
            check_inline_comm `Feature_mathml_inl comm elem

        | Ast.Code txt ->
            let elem attr dict =
                let lang = Style.consume1 dict (Lang_hnd, None) in
                let data = Camlhighlight_parser.from_string ?lang txt in
                let hilite = Hilite.make lang data false in
                Monad.return [Inline.code ~attr hilite] in
            check_inline_comm `Feature_code comm elem

        | Ast.Glyph (href, alt) ->
            let elem attr _ =
                Monad.return [Inline.glyph ~attr href alt] in
            check_inline_comm `Feature_glyph comm elem

        | Ast.Bold astseq ->
            let elem attr _ =
                convert_seq_aux ~comm ~context ~depth ~args is_ref astseq >>= fun seq ->
                Monad.return [Inline.bold ~attr seq] in
            check_inline_comm `Feature_bold comm elem

        | Ast.Emph astseq ->
            let elem attr _ =
                convert_seq_aux ~comm ~context ~depth ~args is_ref astseq >>= fun seq ->
                Monad.return [Inline.emph ~attr seq] in
            check_inline_comm `Feature_emph comm elem

        | Ast.Mono astseq ->
            let elem attr _ =
                convert_seq_aux ~comm ~context ~depth ~args is_ref astseq >>= fun seq ->
                Monad.return [Inline.mono ~attr seq] in
            check_inline_comm `Feature_mono comm elem

        | Ast.Caps astseq ->
            let elem attr _ =
                convert_seq_aux ~comm ~context ~depth ~args is_ref astseq >>= fun seq ->
                Monad.return [Inline.caps ~attr seq] in
            check_inline_comm `Feature_caps comm elem

        | Ast.Ins astseq ->
            let elem attr _ =
                convert_seq_aux ~comm ~context ~depth ~args is_ref astseq >>= fun seq ->
                Monad.return [Inline.ins ~attr seq] in
            check_inline_comm `Feature_ins comm elem

        | Ast.Del astseq ->
            let elem attr _ =
                convert_seq_aux ~comm ~context ~depth ~args is_ref astseq >>= fun seq ->
                Monad.return [Inline.del ~attr seq] in
            check_inline_comm `Feature_del comm elem

        | Ast.Sup astseq ->
            let elem attr _ =
                convert_seq_aux ~comm ~context ~depth ~args is_ref astseq >>= fun seq ->
                Monad.return [Inline.sup ~attr seq] in
            check_inline_comm `Feature_sup comm elem

        | Ast.Sub astseq ->
            let elem attr _ =
                convert_seq_aux ~comm ~context ~depth ~args is_ref astseq >>= fun seq ->
                Monad.return [Inline.sub ~attr seq] in
            check_inline_comm `Feature_sub comm elem

        | Ast.Mbox astseq ->
            let elem attr _ =
                convert_seq_aux ~comm ~context ~depth ~args is_ref astseq >>= fun seq ->
                Monad.return [Inline.mbox ~attr seq] in
            check_inline_comm `Feature_mbox comm elem

        | Ast.Span astseq ->
            let elem attr _ =
                convert_seq_aux ~comm ~context ~depth ~args is_ref astseq >>= fun seq ->
                Monad.return [Inline.span ~attr seq] in
            check_inline_comm `Feature_span comm elem

        | Ast.Link (href, maybe_astseq) when not is_ref ->
            let elem attr _ =
                monadic_maybe (convert_seq_aux ~comm ~context ~depth ~args true) maybe_astseq >>= fun maybe_seq ->
                Monad.return [Inline.link ~attr href maybe_seq] in
            check_inline_comm `Feature_link comm elem

        | Ast.See refs when not is_ref ->
            let elem attr _ =
                let target_checker = function
                    | Target.Note_target _ -> `Valid_target
                    | _                    -> `Wrong_target Error.Target_note in
                List.iter (add_pointer target_checker comm) refs;
                match refs with
                    | _::_ ->
                        has_notes_refs := true;
                        Monad.return [Inline.see ~attr refs]
                    | [] ->
                        add_error comm Error.Empty_list;
                        Monad.return [] in
            check_inline_comm `Feature_see comm elem

        | Ast.Cite refs when not is_ref ->
            let elem attr _ =
                let target_checker = function
                    | Target.Bib_target _ -> `Valid_target
                    | _                   -> `Wrong_target Error.Target_bib in
                List.iter (add_pointer target_checker comm) refs;
                match refs with
                    | _::_ ->
                        has_bibliography_refs := true;
                        Monad.return [Inline.cite ~attr refs]
                    | [] ->
                        add_error comm Error.Empty_list;
                        Monad.return [] in
            check_inline_comm `Feature_cite comm elem

        | Ast.Dref (pointer, maybe_astseq) when not is_ref ->
            let elem attr _ =
                let target_checker = function
                    | Target.Visible_target (Target.Custom_target (_, _, `None_given))
                    | Target.Visible_target (Target.Wrapper_target (_, `None_given))
                    | Target.Visible_target (Target.Part_target `None_given)
                    | Target.Visible_target (Target.Section_target (_, `None_given)) -> `Empty_target
                    | Target.Visible_target _                                        -> `Valid_target
                    | _                                                              -> `Wrong_target Error.Target_label in
                add_pointer target_checker comm pointer;
                monadic_maybe (convert_seq_aux ~comm ~context ~depth ~args true) maybe_astseq >>= fun maybe_seq ->
                Monad.return [Inline.dref ~attr pointer maybe_seq] in
            check_inline_comm `Feature_dref comm elem

        | Ast.Sref (pointer, maybe_astseq) when not is_ref ->
            let elem attr _ =
                let target_checker = function
                    | Target.Visible_target (Target.Custom_target (_, _, `None_given))
                    | Target.Visible_target (Target.Wrapper_target (_, `None_given))
                    | Target.Visible_target (Target.Part_target `None_given)
                    | Target.Visible_target (Target.Section_target (_, `None_given)) -> `Empty_target
                    | Target.Visible_target _                                        -> `Valid_target
                    | _                                                              -> `Wrong_target Error.Target_label in
                add_pointer target_checker comm pointer;
                monadic_maybe (convert_seq_aux ~comm ~context ~depth ~args true) maybe_astseq >>= fun maybe_seq ->
                Monad.return [Inline.sref ~attr pointer maybe_seq] in
            check_inline_comm `Feature_sref comm elem

        | Ast.Mref (pointer, astseq) when not is_ref ->
            let elem attr _ =
                let target_checker = function
                    | Target.Visible_target _ -> `Valid_target
                    | _                       -> `Wrong_target Error.Target_label in
                add_pointer target_checker comm pointer;
                convert_seq_aux ~comm ~context ~depth ~args true astseq >>= fun seq ->
                Monad.return [Inline.mref ~attr pointer seq] in
            check_inline_comm `Feature_mref comm elem

        | Ast.Macroarg raw_num ->
            let elem attr _ = match args with
                | None ->
                    let msg = Error.Invalid_macro_argument_context in
                    add_error comm msg;
                    Monad.return []
                | Some x ->
                    try
                        let num = (int_of_string raw_num) - 1 in
                        Monad.return (List.at x num)
                    with
                        | Failure _
                        | Invalid_argument _ ->
                            let msg = Error.Invalid_macro_argument_number (raw_num, List.length x) in
                            add_error comm msg;
                            Monad.return [] in
            check_inline_comm `Feature_macroarg comm elem

        | Ast.Macrocall (name, arglist) ->
            let elem attr _ =
                try
                    let (macro_nargs, macro_astseq) = Hashtbl.find macros name in
                    if macro_nargs <> List.length arglist
                    then
                        let msg = Error.Invalid_macro_call (name, List.length arglist, macro_nargs) in
                        add_error comm msg;
                        Monad.return []
                    else 
                        let (context_comm, depth) = context in
                        match idiosyncrasies.max_macro_depth with
                            | None ->
                                let context = (context_comm, depth+1) in
                                monadic_map (convert_inline_list ~comm ~context ~depth ~args is_ref) arglist >>= fun new_arglist ->
                                convert_inline_list ~comm ~context ~depth ~args:(Some new_arglist) is_ref macro_astseq
                            | Some num when depth < num ->
                                let context = (context_comm, depth+1) in
                                monadic_map (convert_inline_list ~comm ~context ~depth ~args is_ref) arglist >>= fun new_arglist ->
                                convert_inline_list ~comm ~context ~depth ~args:(Some new_arglist) is_ref macro_astseq
                            | Some num ->
                                let msg = Error.Excessive_macro_depth num in
                                add_error comm msg;
                                Monad.return []
                with
                    | Not_found ->
                        let msg = Error.Undefined_macro name in
                        add_error comm msg;
                        Monad.return [] in
            check_inline_comm `Feature_macrocall comm elem

        | Ast.Extcomm_inl (tag, pattern) ->
            let elem attr _ =
                let inlfun = List.assoc tag inline_extcomms in
                convert_extcomm_inl comm (pattern, inlfun) >>= function
                    | `Okay (astseq, ghosts) ->
                        convert_ghost_frag ~comm ghosts >>= fun () ->
                        convert_seq_aux ~comm ~context ~depth ~args is_ref astseq
                    | `Error xs ->
                        List.iter (fun error -> errors := error :: !errors) xs;
                        Monad.return [] in
            check_inline_comm (`Feature_extcomm_inl tag) comm elem

        | _ ->
            add_error comm Error.Unexpected_inline;
            Monad.return []


    and convert_extcomm_inl comm = function
        | (Ast.Inlpat_empty, Ext.Inlfun_empty f)                                    -> f comm
        | (Ast.Inlpat_seq astseq, Ext.Inlfun_seq f)                                 -> f comm astseq
        | (Ast.Inlpat_raw txt, Ext.Inlfun_raw (_, f))                               -> f comm txt
        | (Ast.Inlpat_raw_raw (txt1, txt2), Ext.Inlfun_raw_raw (_, _, f))           -> f comm txt1 txt2
        | (Ast.Inlpat_raw_seq (txt, astseq), Ext.Inlfun_raw_seq (_, f))             -> f comm txt astseq
        | (Ast.Inlpat_raw_seqopt (txt, maybe_astseq), Ext.Inlfun_raw_seqopt (_, f)) -> f comm txt maybe_astseq
        | _                                                                         -> assert false


    and convert_inline_list ~comm ~context ~depth ~args is_ref astseq = match idiosyncrasies.max_inline_depth with
        | Some max when depth >= max ->
            let msg = Error.Excessive_inline_depth max in
            add_error comm msg;
            Monad.return [dummy_inline]
        | _ ->
            let coalesce_plain seq =
                let rec coalesce_plain_aux accum = function
                    | {Inline.inl = Inline.Plain txt1; attr} :: {Inline.inl = Inline.Plain txt2; _} :: tl ->
                        let agg = Inline.plain ~attr (txt1 ^ txt2) in
                        coalesce_plain_aux accum (agg :: tl)
                    | hd :: tl ->
                        coalesce_plain_aux (hd :: accum) tl
                    | [] ->
                        accum in
                List.rev (coalesce_plain_aux [] seq) in
            let trim_whitespace seq =
                let rec aux accum = function
                    | [{Inline.inl = Inline.Plain txt; attr}] -> (Inline.plain ~attr (String.rstrip txt)) :: accum
                    | hd :: tl                                -> aux (hd :: accum) tl
                    | []                                      -> accum in
                match seq with
                    | [{Inline.inl = Inline.Plain txt; attr}]     -> [Inline.plain ~attr (String.strip txt)]
                    | {Inline.inl = Inline.Plain txt; attr} :: tl -> (Inline.plain ~attr (String.lstrip txt)) :: List.rev (aux [] tl)
                    | xs                                          -> List.rev (aux [] xs) in
            monadic_map (convert_inline ~context ~depth:(depth+1) ~args is_ref) astseq >>= fun seq ->
            let seq = List.flatten seq in
            let seq = if macros_authorised || expand_entities then coalesce_plain seq else seq in
            let seq = if depth = 0 then trim_whitespace seq else seq in
            match seq with
                | [] ->
                    add_error comm Error.Empty_sequence;
                    Monad.return [dummy_inline]
                | xs ->
                    Monad.return xs


    and convert_seq_aux ~comm ~context ~depth ~args is_ref astseq =
        convert_inline_list ~comm ~context ~depth ~args is_ref astseq


    and convert_seq ~comm ?args seq =
        convert_seq_aux ~comm ~context:(comm, 0) ~depth:0 ~args false seq


    (****************************************************************************)
    (* Compilation functions for tabular environment.                           *)
    (****************************************************************************)

    and convert_tabular tab_comm tcols asttab =

        let ncols = match tcols with Some x -> Some (Array.length x) | None -> None in
        let cols_per_row = ref [] in

        let convert_cell (cell_comm, maybe_astseq) =
            let f attr style_parsing =
                let cellfmt = Style.consume1 style_parsing (Cell_hnd, None) in
                let colspan = match cellfmt with
                    | Some {colspan; _} -> colspan
                    | None              -> 1 in
                monadic_maybe (convert_seq ~comm:cell_comm) maybe_astseq >>= fun maybe_seq ->
                Monad.return (colspan, Tabular.make_cell ~attr ?cellfmt maybe_seq) in
            check_comm `Feature_cell cell_comm f in

        let convert_row (row_comm, row) =
            let rowspan = ref 0 in
            let converter raw_cell =
                convert_cell raw_cell >>= function
                    | Some (colspan, cell) ->
                        let () = rowspan := !rowspan + colspan in
                        Monad.return cell
                    | None ->
                        Monad.return dummy_cell in
            let tab_row = match row with
                | _::_ -> monadic_map converter row >|= Tabular.make_row
                | []   -> assert false in
            match ncols with
                | Some n when n <> !rowspan ->
                    let msg = Error.Invalid_column_number (tab_comm.comm_tag, tab_comm.comm_linenum, !rowspan, n) in
                    add_error row_comm msg;
                    tab_row
                | Some _ ->
                    tab_row
                | None ->
                    cols_per_row := !rowspan :: !cols_per_row;
                    tab_row in

        let convert_group feature (comm, rows) =
            check_inline_comm feature comm (fun _ _ -> Monad.return []) >>= fun _ ->
            match rows with
                | _::_ -> monadic_map convert_row rows >|= Tabular.make_group
                | []   -> assert false in

        monadic_maybe (convert_group `Feature_thead) asttab.thead >>= fun thead ->
        monadic_maybe (convert_group `Feature_tfoot) asttab.tfoot >>= fun tfoot ->
        begin match asttab.tbodies with
            | _::_ -> monadic_map (convert_group `Feature_tbody) asttab.tbodies >|= Tabular.make ?tcols ?thead ?tfoot
            | []   -> assert false
        end >>= fun tabular ->
        if not (all_equal !cols_per_row)
        then begin
            let msg = Error.Mismatched_column_numbers (List.rev !cols_per_row) in
            add_error tab_comm msg
        end;
        Monad.return tabular


    (****************************************************************************)
    (* Compilation functions for document blocks.                               *)
    (****************************************************************************)

    and convert_block ~minipaged ~depth allowed (comm, astblk) = match astblk with

        | Ast.Paragraph astseq when Blkcat.subtype [`Paragraph_blk; `Embeddable_blk] allowed ->
            let elem attr _ =
                convert_seq ~comm astseq >>= fun seq ->
                Monad.return [Block.paragraph ~attr seq] in
            check_block_comm `Feature_paragraph comm elem

        | Ast.Itemize astfrags when Blkcat.subtype [`Embeddable_blk] allowed ->
            let elem attr _ = convert_frag_of_anon_frags ~comm ~cons:(Block.itemize ~attr) ~minipaged ~depth allowed astfrags in
            check_block_comm `Feature_itemize comm elem

        | Ast.Enumerate astfrags when Blkcat.subtype [`Embeddable_blk] allowed ->
            let elem attr _ = convert_frag_of_anon_frags ~comm ~cons:(Block.enumerate ~attr) ~minipaged ~depth allowed astfrags in
            check_block_comm `Feature_enumerate comm elem

        | Ast.Description astfrags when Blkcat.subtype [`Embeddable_blk] allowed ->
            let elem attr _ = convert_frag_of_desc_frags ~comm ~cons:(Block.description ~attr) ~minipaged ~depth allowed astfrags in
            check_block_comm `Feature_description comm elem

        | Ast.Qanda astfrags when Blkcat.subtype [`Embeddable_blk] allowed ->
            let elem attr _ = convert_frag_of_qanda_frags ~comm ~cons:(Block.qanda ~attr) ~minipaged ~depth allowed astfrags in
            check_block_comm `Feature_qanda comm elem

        | Ast.Verse astfrag when Blkcat.subtype [`Embeddable_blk] allowed ->
            let elem attr _ =
                convert_frag_aux ~comm ~minipaged ~depth `Paragraph_blk astfrag >>= fun frag ->
                Monad.return [Block.verse ~attr frag] in
            check_block_comm `Feature_verse comm elem

        | Ast.Quote astfrag when Blkcat.subtype [`Quotable_blk] allowed ->
            let elem attr _ =
                convert_frag_aux ~comm ~minipaged ~depth `Quotable_blk astfrag >>= fun frag ->
                Monad.return [Block.quote ~attr frag] in
            check_block_comm `Feature_quote comm elem

        | Ast.Mathtex_blk txt when Blkcat.subtype [`Equation_blk; `Embeddable_blk] allowed ->
            let elem attr _ =
                let trimmed = Literal_input.trim txt in
                Monad.return (convert_mathtex (Block.math_blk ~attr) comm trimmed) in
            check_block_comm `Feature_mathtex_blk comm elem

        | Ast.Mathml_blk txt when Blkcat.subtype [`Equation_blk; `Embeddable_blk] allowed ->
            let elem attr _ =
                let trimmed = Literal_input.trim txt in
                Monad.return (convert_mathml (Block.math_blk ~attr) comm trimmed) in
            check_block_comm `Feature_mathml_blk comm elem

        | Ast.Source txt when Blkcat.subtype [`Printout_blk; `Embeddable_blk] allowed ->
            let elem attr dict =
                let (lang, linenums) = Style.consume2 dict (Lang_hnd, None) (Linenums_hnd, false) in
                let trimmed = Literal_input.trim txt in
                let data = Camlhighlight_parser.from_string ?lang trimmed in
                let hilite = Hilite.make lang data linenums in
                let () = if trimmed = "" then add_error comm Error.Empty_source in
                Monad.return [Block.source ~attr hilite] in
            check_block_comm `Feature_source comm elem

        | Ast.Tabular asttab when Blkcat.subtype [`Table_blk; `Embeddable_blk] allowed ->
            let elem attr dict =
                let tcols = Style.consume1 dict (Cols_hnd, None) in
                convert_tabular comm tcols asttab >>= fun tab ->
                Monad.return [Block.tabular ~attr tab] in
            check_block_comm `Feature_tabular comm elem

        | Ast.Subpage astfrag when Blkcat.subtype [`Figure_blk; `Listable_blk] allowed ->
            let elem attr _ =
                convert_frag_aux ~comm ~minipaged:true ~depth `Super_blk astfrag >>= fun frag ->
                Monad.return [Block.subpage ~attr frag] in
            check_block_comm `Feature_subpage comm elem

        | Ast.Verbatim txt when Blkcat.subtype [`Figure_blk; `Embeddable_blk] allowed ->
            let elem attr _ =
                let trimmed = Literal_input.trim txt in
                let () = if trimmed = "" then add_error comm Error.Empty_verbatim in
                Monad.return [Block.verbatim ~attr trimmed] in
            check_block_comm `Feature_verbatim comm elem

        | Ast.Picture (href, alt) when Blkcat.subtype [`Figure_blk; `Embeddable_blk] allowed ->
            let elem attr dict =
                let width = Style.consume1 dict (Width_hnd, None) in
                Monad.return [Block.picture ~attr href alt width] in
            check_block_comm `Feature_picture comm elem

        | Ast.Pullquote (maybe_astseq, astfrag) when Blkcat.subtype [`Listable_blk] allowed ->
            let elem attr _ =
                monadic_maybe (convert_seq ~comm) maybe_astseq >>= fun maybe_seq ->
                convert_frag_aux ~comm ~minipaged ~depth `Embeddable_blk astfrag >>= fun frag ->
                Monad.return [Block.pullquote ~attr maybe_seq frag] in
            check_block_comm `Feature_pullquote comm elem

        | Ast.Custom (env, maybe_astseq, astfrag) when Blkcat.subtype [`Listable_blk] allowed ->
            let elem attr _ =
                let bad_order reason =
                    let msg = Error.Misplaced_order_parameter reason in
                    add_error comm msg;
                    Order_input.no_order () in
                try
                    let (kind, used, def) = Hashtbl.find customisations env in
                    let () = if not used then Hashtbl.replace customisations env (kind, true, def) in
                    let order = match (def, comm.comm_order, minipaged) with
                        | Numbered _, None, true             -> bad_order Error.Reason_is_absent_when_mandatory
                        | Numbered (_, counter), None, false -> Order_input.auto_ordinal counter
                        | Numbered _, Some "", _             -> Order_input.no_order ()
                        | Numbered _ , Some other, true      -> make_user_ordinal comm other
                        | Numbered _ , Some other, false     -> bad_order (Error.Reason_is_non_empty_when_forbidden other)
                        | _, None, _                         -> Order_input.no_order ()
                        | _, Some "", _                      -> Order_input.no_order ()
                        | _, Some other, _                   -> bad_order (Error.Reason_is_non_empty_when_forbidden other) in
                    let label = make_label comm (Target.custom env kind order) in
                    let custom_maker = match def with
                        | Anonymous    -> Custom.anonymous
                        | Unnumbered _ -> Custom.unnumbered
                        | Numbered _   -> Custom.numbered in
                    let data = custom_maker env label order in
                    let (block_maker, allowed) = match kind with
                        | Custom.Boxout  -> (Block.boxout ~attr (Custom.Boxout.make data), Blkcat.min allowed `Quotable_blk)
                        | Custom.Theorem -> (Block.theorem ~attr (Custom.Theorem.make data), `Embeddable_blk) in
                    monadic_maybe (convert_seq ~comm) maybe_astseq >>= fun maybe_seq ->
                    convert_frag_aux ~comm ~minipaged ~depth allowed astfrag >>= fun frag ->
                    Monad.return [block_maker maybe_seq frag]
                with
                    | Not_found ->
                        let msg = Error.Undefined_custom env in
                        add_error comm msg;
                        Monad.return [] in
            check_block_comm ~maybe_minipaged:(Some minipaged) `Feature_custom comm elem

        | Ast.Equation (maybe_astseq, astblk) when Blkcat.subtype [`Listable_blk] allowed ->
            let elem attr _ =
                convert_wrapper comm equation_counter Wrapper.Equation maybe_astseq >>= fun wrapper ->
                convert_block ~minipaged ~depth `Equation_blk astblk >>= function
                    | [blk] -> Monad.return [Block.equation ~attr wrapper blk]
                    | _     -> Monad.return [] in
            check_block_comm ~maybe_minipaged:(Some minipaged) `Feature_equation comm elem

        | Ast.Printout (maybe_astseq, astblk) when Blkcat.subtype [`Listable_blk] allowed ->
            let elem attr _ =
                convert_wrapper comm printout_counter Wrapper.Printout maybe_astseq >>= fun wrapper ->
                convert_block ~minipaged ~depth `Printout_blk astblk >>= function
                    | [blk] -> Monad.return [Block.printout ~attr wrapper blk]
                    | _     -> Monad.return [] in
            check_block_comm ~maybe_minipaged:(Some minipaged) `Feature_printout comm elem

        | Ast.Table (maybe_astseq, astblk) when Blkcat.subtype [`Listable_blk] allowed ->
            let elem attr _ =
                convert_wrapper comm table_counter Wrapper.Table maybe_astseq >>= fun wrapper ->
                convert_block ~minipaged ~depth `Table_blk astblk >>= function
                    | [blk] -> Monad.return [Block.table ~attr wrapper blk]
                    | _     -> Monad.return [] in
            check_block_comm ~maybe_minipaged:(Some minipaged) `Feature_table comm elem

        | Ast.Figure (maybe_astseq, astblk) when Blkcat.subtype [`Listable_blk] allowed ->
            let elem attr _ =
                convert_wrapper comm figure_counter Wrapper.Figure maybe_astseq >>= fun wrapper ->
                convert_block ~minipaged ~depth `Figure_blk astblk >>= function
                    | [blk] -> Monad.return [Block.figure ~attr wrapper blk]
                    | _     -> Monad.return [] in
            check_block_comm ~maybe_minipaged:(Some minipaged) `Feature_figure comm elem

        | Ast.Part astseq when allowed = `Super_blk ->
            let elem attr _ =
                let order = match comm.comm_order with
                    | None       -> Order_input.auto_ordinal part_counter
                    | Some ""    -> Order_input.no_order ()
                    | Some other -> make_user_ordinal comm other in
                let label = make_label comm (Target.part order) in
                convert_seq ~comm astseq >>= fun seq ->
                let heading = Heading.part label order seq in
                let block = Block.heading ~attr heading in
                let () = if not minipaged then add_toc_entry heading in
                Monad.return [block] in
            check_block_comm ~maybe_minipaged:(Some minipaged) `Feature_part comm elem

        | Ast.Appendix when allowed = `Super_blk ->
            let elem attr _ =
                let order = Order_input.no_order () in
                let label = make_label comm (Target.part order) in
                let heading = Heading.appendix label in
                let block = Block.heading ~attr heading in
                let () = if not minipaged then add_toc_entry heading in
                let () = appendixed := true in
                Monad.return [block] in
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
                        let msg = Error.Invalid_section_level level in
                        add_error comm msg;
                        Level.section 1 in
                let order = match comm.comm_order with
                    | None       -> Order_input.auto_hierarchical level' counter
                    | Some ""    -> Order_input.no_order ()
                    | Some other -> make_user_hierarchical comm level' other in
                let label = make_label comm (Target.section location order) in
                convert_seq ~comm astseq >>= fun seq ->
                let heading = Heading.section label order location level' seq in
                let block = Block.heading ~attr heading in
                let () = if not minipaged then add_toc_entry heading in
                Monad.return [block] in
            let feature = match level with
                | 1 -> `Feature_section1
                | 2 -> `Feature_section2
                | 3 -> `Feature_section3
                | 4 -> `Feature_section4
                | 5 -> `Feature_section5
                | _ -> `Feature_section6 in
            check_block_comm ~maybe_minipaged:(Some minipaged) feature comm elem

        | Ast.Bibliography when allowed = `Super_blk ->
            has_bibliography_section := true;
            convert_preset_sectional ~tocable:true ~minipaged Heading.bibliography `Feature_bibliography comm

        | Ast.Notes when allowed = `Super_blk ->
            has_notes_section := true;
            convert_preset_sectional ~tocable:true ~minipaged Heading.notes `Feature_notes comm 

        | Ast.Toc when allowed = `Super_blk ->
            convert_preset_sectional ~tocable:false ~minipaged Heading.toc `Feature_toc comm

        | Ast.Title (level, astseq) when allowed = `Super_blk  ->
            let elem attr _ =
                let level' =
                    try Level.title level
                    with Invalid_argument _ -> 
                        let msg = Error.Invalid_title_level level in
                        add_error comm msg;
                        Level.title 1 in
                convert_seq ~comm astseq >>= fun seq ->
                Monad.return [Block.title ~attr level' seq] in
            let feature = match level with
                | 1 -> `Feature_title1
                | _ -> `Feature_title2 in
            check_block_comm feature comm elem

        | Ast.Abstract astfrag when allowed = `Super_blk ->
            let elem attr _ =
                convert_frag_aux ~comm ~minipaged ~depth `Embeddable_blk astfrag >>= fun frag ->
                Monad.return [Block.abstract ~attr frag] in
            check_block_comm `Feature_abstract comm elem

        | Ast.Rule when allowed = `Super_blk ->
            let elem attr _ = Monad.return [Block.rule ~attr ()] in
            check_block_comm `Feature_rule comm elem

        | Ast.Bib bib ->
            let elem attr _ =
                let (author_comm, author_astseq) = bib.author
                and (title_comm, title_astseq) = bib.title
                and (resource_comm, resource_astseq) = bib.resource in
                let order = Order_input.auto_ordinal bib_counter in
                let label = make_label comm (Target.bib order) in
                check_comm `Feature_bib_author author_comm (fun _ _ -> convert_seq ~comm author_astseq) >>= fun author ->
                check_comm `Feature_bib_title title_comm (fun _ _ -> convert_seq ~comm title_astseq) >>= fun title ->
                check_comm `Feature_bib_resource resource_comm (fun _ _ -> convert_seq ~comm resource_astseq) >>= fun resource ->
                match (author, title, resource) with
                    | (Some author, Some title, Some resource) ->
                        let bib = Bib.make label order author title resource in
                        bibs := bib :: !bibs;
                        Monad.return []
                    | _ ->
                        Monad.return [] in
            check_block_comm `Feature_bib comm elem

        | Ast.Note astfrag ->
            let elem attr _ =
                let order = Order_input.auto_ordinal note_counter in
                let label = make_label comm (Target.note order) in
                convert_frag_aux ~comm ~minipaged:true ~depth `Listable_blk astfrag >>= fun frag ->
                let note = Note.make label order frag in
                notes := note :: !notes;
                Monad.return [] in
            check_block_comm `Feature_note comm elem

        | Ast.Macrodef (name, nargs, astseq) ->
            let elem attr _ =
                if not (Identifier_input.matches_macrodef name)
                then begin
                    let msg = Error.Invalid_macro name in
                    add_error comm msg
                end;
                let num_args =
                    try int_of_string nargs
                    with Failure _ ->
                        let msg = Error.Invalid_macro_nargs (name, nargs) in
                        add_error comm msg; 0 in
                let errors_before = !errors in
                convert_seq ~comm ~args:(List.make num_args [dummy_inline]) astseq >>= fun _ ->
                let errors_after = !errors in
                begin
                    if Hashtbl.mem macros name
                    then
                        let msg = Error.Duplicate_macro name in
                        add_error comm msg
                    else
                        let new_astseq = if errors_after = errors_before then astseq else [(comm, Ast.Linebreak)] in
                        Hashtbl.add macros name (num_args, new_astseq)
                end;
                Monad.return [] in
            check_block_comm `Feature_macrodef comm elem

        | Ast.Boxoutdef (env, maybe_caption, maybe_counter_name) ->
            let elem attr _ = convert_customdef comm env Custom.Boxout maybe_caption maybe_counter_name in
            check_block_comm `Feature_boxoutdef comm elem

        | Ast.Theoremdef (env, caption, maybe_counter_name) ->
            let elem attr _ = convert_customdef comm env Custom.Theorem (Some caption) maybe_counter_name in
            check_block_comm `Feature_theoremdef comm elem

        | Ast.Extcomm_blk (tag, pattern) when Blkcat.subtype (List.assoc tag block_extcomms |> snd) allowed ->
            let elem attr _ =
                (* Note that we use List.assoc again. Hopefully OCaml will support "with guards" in the near future. *)
                let (blkfun, _) = List.assoc tag block_extcomms in
                convert_extcomm_blk comm (pattern, blkfun) >>= function
                    | `Okay (astfrag, ghosts) ->
                        convert_ghost_frag ~comm ghosts >>= fun () ->
                        convert_frag_aux ~comm ~minipaged ~depth allowed astfrag
                    | `Error xs ->
                        List.iter (fun error -> errors := error :: !errors) xs;
                        Monad.return [] in
            check_block_comm (`Feature_extcomm_blk tag) comm elem

        | _ ->
            let msg = Error.Unexpected_block allowed in
            add_error comm msg;
            Monad.return [dummy_block]


    and convert_preset_sectional ~tocable ~minipaged cons feature comm = 
        let elem attr _ =
            let order = Order_input.no_order () in
            let label = make_label comm (Target.section Heading.Mainbody order) in
            let heading = cons label in
            let block = Block.heading ~attr heading in
            let () = if tocable && not minipaged then add_toc_entry heading in
            Monad.return [block] in
        check_block_comm ~maybe_minipaged:(Some minipaged) `Feature_notes comm elem


    and convert_wrapper comm counter kind maybe_astseq =
        let order = match comm.comm_order with
            | None       -> Order_input.auto_ordinal counter
            | Some ""    -> Order_input.no_order ()
            | Some thing -> make_user_ordinal comm thing in
        let label = make_label comm (Target.wrapper kind order) in
        monadic_maybe (convert_seq ~comm) maybe_astseq >>= fun maybe_seq ->
        match (order, maybe_seq) with
            | (`None_given, None) ->
                let msg = Error.Invalid_wrapper kind in
                add_error comm msg;
                Monad.return (Wrapper.Unordered (label, [dummy_inline]))
            | (`None_given, Some seq) ->
                Monad.return (Wrapper.Unordered (label, seq))
            | (`Auto_given _ as o, maybe_seq)
            | (`User_given _ as o, maybe_seq) ->
                Monad.return (Wrapper.Ordered (label, o, maybe_seq))


    and convert_customdef comm env kind maybe_caption maybe_counter_name =
        if not (Identifier_input.matches_customdef env)
        then begin
            let msg = Error.Invalid_custom env in
            add_error comm msg;
            Monad.return []
        end
        else if Hashtbl.mem customisations env
        then begin
            let msg = Error.Duplicate_custom env in
            add_error comm msg;
            Monad.return []
        end
        else match (maybe_caption, maybe_counter_name) with
            | (None, None) ->
                let data = (kind, false, Anonymous) in
                Hashtbl.add customisations env data;
                Monad.return []
            | (None, Some counter_name) ->
                let msg = Error.Unexpected_counter counter_name in
                add_error comm msg;
                Monad.return []
            | (Some astseq, None) ->
                convert_seq ~comm astseq >>= fun seq ->
                let data = (kind, false, Unnumbered seq) in
                Hashtbl.add customisations env data;
                Monad.return []
            | (Some astseq, Some counter_name) when not (Hashtbl.mem custom_counters counter_name) ->
                if Identifier_input.matches_counter counter_name
                then begin
                    let counter = Order_input.ordinal_counter () in
                    convert_seq ~comm astseq >>= fun seq ->
                    let data = (kind, false, Numbered (seq, counter)) in
                    Hashtbl.add custom_counters counter_name (kind, counter);
                    Hashtbl.add customisations env data;
                    Monad.return []
                end
                else begin
                    let msg = Error.Invalid_counter counter_name in
                    add_error comm msg;
                    Monad.return []
                end
            | (Some astseq, Some counter_name) -> match Hashtbl.find custom_counters counter_name with
                | (k, _) when k <> kind ->
                    let msg = Error.Mismatched_counter counter_name in
                    add_error comm msg;
                    Monad.return []
                | (_, counter) ->
                    convert_seq ~comm astseq >>= fun seq ->
                    let data = (kind, false, Numbered (seq, counter)) in
                    Hashtbl.add customisations env data;
                    Monad.return []


    and convert_frag_of_anon_frags ~comm ~cons ~minipaged ~depth allowed astfrags =
        let conv (comm, astfrag) =
            let elem attr _ = convert_frag_aux ~comm ~minipaged ~depth (Blkcat.min allowed `Listable_blk) astfrag in
            check_comm `Feature_item comm elem in
        monadic_filter_map conv astfrags >>= function
            | [] ->
                add_error comm Error.Empty_fragment;
                Monad.return [dummy_block]
            | frags ->
                Monad.return [cons frags]


    and convert_frag_of_desc_frags ~comm ~cons ~minipaged ~depth allowed astfrags =
        let conv (comm, astseq, astfrag) =
            let elem attr _ =
                convert_seq ~comm astseq >>= fun seq ->
                convert_frag_aux ~comm ~minipaged ~depth (Blkcat.min allowed `Listable_blk) astfrag >>= fun frag ->
                Monad.return (seq, frag) in
            check_comm `Feature_item comm elem in
        monadic_filter_map conv astfrags >>= function
            | [] ->
                add_error comm Error.Empty_fragment;
                Monad.return [dummy_block]
            | frags ->
                Monad.return [cons frags]


    and convert_frag_of_qanda_frags ~comm ~cons ~minipaged ~depth allowed astfrags =
        let conv (comm, qanda, astfrag) =
            begin match qanda with
                | New_questioner maybe_astseq ->
                    monadic_maybe (convert_seq ~comm) maybe_astseq >>= fun maybe_seq ->
                    Monad.return (`Feature_question, fun () -> Qanda.New_questioner maybe_seq)
                | New_answerer maybe_astseq ->
                    monadic_maybe (convert_seq ~comm) maybe_astseq >>= fun maybe_seq ->
                    Monad.return (`Feature_answer, fun () -> Qanda.New_answerer maybe_seq)
                | Same_questioner ->
                    Monad.return (`Feature_rquestion, fun () -> Qanda.Same_questioner)
                | Same_answerer ->
                    Monad.return (`Feature_ranswer, fun () -> Qanda.Same_answerer)
            end >>= fun (feature, qanda_maker) ->
            let elem attr _ =
                let qanda = qanda_maker () in
                convert_frag_aux ~comm ~minipaged ~depth (Blkcat.min allowed `Listable_blk) astfrag >>= fun frag ->
                Monad.return [(qanda, frag)] in
            check_comm feature comm elem in
        monadic_filter_map conv astfrags >>= function
            | [] ->
                add_error comm Error.Empty_fragment;
                Monad.return [dummy_block]
            | frags ->
                Monad.return [cons (List.flatten frags)]


    and convert_extcomm_blk comm = function
        | (Ast.Blkpat_empty, Ext.Blkfun_empty f)                          -> f comm
        | (Ast.Blkpat_seq astseq, Ext.Blkfun_seq f)                       -> f comm astseq
        | (Ast.Blkpat_lit txt, Ext.Blkfun_lit f)                          -> f comm txt
        | (Ast.Blkpat_frag astfrag, Ext.Blkfun_frag f)                    -> f comm astfrag
        | (Ast.Blkpat_raw txt, Ext.Blkfun_raw (_, f))                     -> f comm txt
        | (Ast.Blkpat_raw_raw (txt1, txt2), Ext.Blkfun_raw_raw (_, _, f)) -> f comm txt1 txt2
        | _                                                               -> assert false


    and convert_ghost_frag ~comm astfrag =
        let conv astblk = convert_block ~minipaged:false ~depth:0 `Super_blk astblk in
        monadic_map conv astfrag >>= fun frags ->
        match List.flatten frags with
            | [] -> Monad.return ()
            | _  -> Monad.fail (Internal_extension_error comm)


    and convert_frag_aux ?comm ~minipaged ~depth allowed astfrag =
        let conv = match idiosyncrasies.max_block_depth with
            | None -> fun astblk ->
                convert_block ~minipaged ~depth allowed astblk
            | Some max -> fun astblk ->
                if depth >= max
                then
                    let (comm, _) = astblk in
                    let msg = Error.Excessive_block_depth max in
                    add_error comm msg;
                    Monad.return [dummy_block]
                else
                    convert_block ~minipaged ~depth:(depth+1) allowed astblk in
        monadic_map conv astfrag >>= function
            | [] ->
                let (tag, linenum) = match comm with
                    | Some comm -> (comm.comm_tag, Some comm.comm_linenum)
                    | None      -> (None, None) in
                errors := (linenum, tag, Error.Empty_fragment) :: !errors;
                Monad.return [dummy_block]
            | frag ->
                Monad.return (List.flatten frag)


    and convert_frag astfrag =
        convert_frag_aux ~minipaged:false ~depth:0 `Super_blk astfrag in


    (****************************************************************************)
    (* Filtering of internal references.                                        *)
    (****************************************************************************)

    let filter_pointers () =
        let filter_pointer (target_checker, comm, label) =
            try
                let target = Hashtbl.find labels (Label.User label) in
                match target_checker target with
                | `Valid_target ->
                    ()
                | `Empty_target ->
                    let msg = Error.Empty_target label in
                    add_error comm msg
                | `Wrong_target expected ->
                    let suggestion = match target with
                        | Target.Visible_target _ -> Error.Target_label
                        | Target.Bib_target _     -> Error.Target_bib
                        | Target.Note_target _    -> Error.Target_note in
                    let msg = Error.Wrong_target (label, expected, suggestion) in
                    add_error comm msg
            with
                Not_found ->
                    let msg = Error.Undefined_target label in
                    add_error comm msg in
        List.iter filter_pointer !pointers in


    (****************************************************************************)
    (* Filtering of customisations: we only save those actually used.           *)
    (****************************************************************************)

    let filter_customisations () =
        let custom = Hashtbl.create (Hashtbl.length customisations) in
        let adder key = function
            | (_, true, Unnumbered seq)
            | (_, true, Numbered (seq, _)) -> Hashtbl.add custom key seq
            | _                            -> () in
        Hashtbl.iter adder customisations;
        custom in


    (****************************************************************************)
    (* Verify whether document with bibs/notes has corresponding section.       *)
    (****************************************************************************)

    let verify_section msg has_refs has_section =
        if has_refs && not has_section
        then errors := (None, None, msg) :: !errors in


    (****************************************************************************)
    (* Wrap-up.                                                                 *)
    (****************************************************************************)

    convert_frag ast >>= fun content ->
    let customs = filter_customisations () in
    let () = filter_pointers () in
    let () = verify_section Error.Missing_bibliography !has_bibliography_refs !has_bibliography_section in
    let () = verify_section Error.Missing_notes !has_notes_refs !has_notes_section in
    let valid = Valid.make ~bibs:(List.rev !bibs) ~notes:(List.rev !notes) ~toc:(List.rev !toc) ~labels ~customs content in
    begin match postprocessor with
        | None   -> Monad.return (!errors, valid)
        | Some p -> Foldmapper.(p.valid p !errors valid)
    end >>= function
        | ([], valid) -> Monad.return (Ambivalent.valid valid)
        | (errors, _) -> Monad.return (Ambivalent.invalid (Invalid.make (contextualize_errors ~sort:true source errors)))
end

