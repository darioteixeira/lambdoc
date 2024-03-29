open Printf
open Lambdoc_prelude
open Lambdoc_document
open Lambdoc_writer
open Valid
open Invalid
open Attr
open Inline
open Block
open Heading
open Hilite
open Writeconv


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type valid_options =
    {
    numbered_paragraphs: bool;
    translations: Translations.t;
    namespace: Html_types.nmtoken;
    prefix: Html_types.nmtoken;
    base_classes: Html_types.nmtokens;
    extra_classes: Html_types.nmtokens;
    }

type invalid_options =
    {
    prefix: Html_types.nmtoken;
    base_classes: Html_types.nmtokens;
    extra_classes: Html_types.nmtokens;
    }


(********************************************************************************)
(** {1 Public functors}                                                         *)
(********************************************************************************)

module Make (Html: Html_sigs.NoWrap) =
struct

open Html


(********************************************************************************)
(** {2 Private modules}                                                         *)
(********************************************************************************)

module Hilite_writer = Camlhighlight_write_html.Make (Html)


(********************************************************************************)
(** {2 Exceptions}                                                              *)
(********************************************************************************)

exception Command_see_with_non_note of Target.t
exception Command_cite_with_non_bib of Target.t
exception Command_ref_with_non_visible_block of Target.t
exception Command_sref_with_non_visible_block of Target.t


(********************************************************************************)
(** {2 Private type definitions}                                                *)
(********************************************************************************)

type name =
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
(** {2 Public type definitions}                                                 *)
(********************************************************************************)

type t = Html_types.div Html.elt

type valid_options_u = valid_options
type valid_options = valid_options_u

type invalid_options_u = invalid_options
type invalid_options = invalid_options_u


(********************************************************************************)
(** {2 Private functions and values}                                            *)
(********************************************************************************)

let maybe f = function
    | Some x -> Some (f x)
    | None   -> None


(********************************************************************************)
(** {2 Public functions and values}                                             *)
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
(** {3 Conversion of valid documents}                                           *)
(********************************************************************************)

let from_valid ?(valid_options = default_valid_options) doc =

    let opts = valid_options in


    (****************************************************************************)
    (*  {4 Fetch fields from doc record}                                        *)
    (****************************************************************************)

    let content = Valid.(doc.content) in
    let bibs = Valid.(doc.bibs) in
    let notes = Valid.(doc.notes) in
    let toc = Valid.(doc.toc) in
    let labels = Valid.(doc.labels) in
    let customs = Valid.(doc.customs) in


    (****************************************************************************)
    (** {4 Document classname prefix}                                           *)
    (****************************************************************************)

    let (!!) =
        let prefix = opts.prefix ^ "_" in
        fun str -> prefix ^ str in

    let (!!!) =
        let prefix = opts.prefix ^ "_class_" in
        fun str -> prefix ^ str in

    let (^^) =
        let prefix = opts.prefix ^ "_" in
        fun s1 s2 -> prefix ^ s1 ^ s2 in


    (****************************************************************************)
    (** {4 Helper functions}                                                    *)
    (****************************************************************************)

    let make_label = function
        | Label.Auto pointer -> opts.namespace ^ ":a:" ^ pointer
        | Label.Manual pointer -> opts.namespace ^ ":m:" ^ pointer in

    let make_link ?(classnames = []) lnk content =
        Html.a ~a:[a_class (!!"lnk" :: classnames); a_href (Html.uri_of_string lnk)] content in

    let make_internal_link ?classnames pointer content =
        make_link ?classnames ("#" ^ (make_label pointer)) content in

    let cons_of_level = function
        | 1 -> Html.h1
        | 2 -> Html.h2
        | 3 -> Html.h3
        | 4 -> Html.h4
        | 5 -> Html.h5
        | 6 -> Html.h6
        | _ -> assert false in

    let class_of_level level = !!("level" ^ string_of_int level) in

    let make_heading cons label orderlst classnames content =
        cons ?a:(Some [a_id (make_label label); a_class classnames]) (orderlst @ [Html.span content]) in

    let make_sectional level label orderlst classnames content =
        make_heading (cons_of_level (level : Level.section :> int)) label orderlst (!!"sec" :: classnames) content in

    let make_floatable forbidden =
        if forbidden then [] else [!!"floatable"] in

    let commafy ~prefix ~suffix xs =
        let rec aux accum = function
            | []       -> assert false
            | [x]      -> pcdata prefix :: List.rev (pcdata suffix :: x :: accum)
            | hd :: tl -> aux (pcdata "," :: hd :: accum) tl in
        aux [] xs in


    (****************************************************************************)
    (** {4 Converters}                                                          *)
    (****************************************************************************)

    let listify_order ?(spanify = false) ?(prespace = false) ?order_prefix order =
        let content = match (order_prefix, order) with
            | (Some p, Some o) -> Some (p ^ o)
            | (None, Some o)   -> Some o
            | _                -> None in
        let bundle = match (content, prespace) with
            | (Some c, true)  -> [Html.entity "#xa0"; pcdata c]
            | (Some c, false) -> [pcdata c]
            | (None, _)       -> []
        in match (bundle, spanify) with
            | ([], _)    -> []
            | (b, true)  -> [Html.span ~a:[a_class [!!"order"]] b]
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


    (****************************************************************************)
    (*  {4 Counters}                                                            *)
    (****************************************************************************)

    let paragraph_counter = ref 0 in


    (****************************************************************************)
    (*  {4 Predefined sequences for Q&A}                                        *)
    (****************************************************************************)

    let last_question_seq = ref None in
    let last_answer_seq = ref None in


    (****************************************************************************)
    (*  {4 Writers for inline context}                                          *)
    (****************************************************************************)

    let rec write_seq seq =
        List.map write_inline seq

    and write_inline {inl; attr} =
        let classnames = List.map (!!!) attr.classnames in
        match inl with

        | Plain txt ->
            Html.pcdata txt

        | Entity txt ->
            Html.entity txt

        | Linebreak ->
            Html.br ~a:[a_class classnames] ()

        | Math_inl math ->
            let html: [> Html_types.span ] Html.elt = Html.Unsafe.data (Math_output.get_mathml math) in
            Html.span ~a:[a_class (!!"math_inl" :: classnames)] [html]

        | Code hilite ->
            Hilite_writer.write_inline ~class_prefix:!!"src_" ~extra_classes:classnames hilite.data

        | Glyph (href, alt, title) ->
            let suffix = match title with Some t -> [a_title t] | None -> [] in
            let uri = Html.uri_of_string href in
            Html.img ~a:(a_class (!!"glyph" :: classnames) :: suffix) ~src:uri ~alt ()

        | Bold seq ->
            Html.b ~a:[a_class (!!"bold" :: classnames)] (write_seq seq)

        | Emph seq ->
            Html.i ~a:[a_class (!!"emph" :: classnames)] (write_seq seq)

        | Mono seq ->
            Html.code ~a:[a_class (!!"mono" :: classnames)] (write_seq seq)

        | Caps seq ->
            Html.span ~a:[a_class (!!"caps" :: classnames)] (write_seq seq)

        | Ins seq ->
            Html.ins ~a:[a_class (!!"ins" :: classnames)] (write_seq seq)

        | Del seq ->
            Html.del ~a:[a_class (!!"del" :: classnames)] (write_seq seq)

        | Sup seq ->
            Html.sup ~a:[a_class (!!"sup" :: classnames)] (write_seq seq)

        | Sub seq ->
            Html.sub ~a:[a_class (!!"sub" :: classnames)] (write_seq seq)

        | Mbox seq ->
            Html.span ~a:[a_class (!!"mbox" :: classnames)] (write_seq seq)

        | Span seq ->
            Html.span ~a:[a_class classnames] (write_seq seq)

        | Link (href, maybe_seq) ->
            let seq = match maybe_seq with
                | Some seq -> seq
                | None     -> [Inline.plain href] in
            make_link ~classnames href (Obj.magic (write_seq seq))

        | See pointers ->
            let link_maker pointer =
                let label = Label.Manual pointer in
                let target = Hashtbl.find labels label in
                match target with
                    | Target.Note order -> make_internal_link label (note_conv order)
                    | _                 -> raise (Command_see_with_non_note target) in
            Html.span ~a:[a_class (!!"see" :: classnames)] (commafy ~prefix:"(" ~suffix:")" (List.map link_maker pointers))

        | Cite pointers ->
            let link_maker pointer =
                let label = Label.Manual pointer in
                let target = Hashtbl.find labels label in
                match target with
                    | Target.Bib order -> make_internal_link label (bib_conv order)
                    | _                -> raise (Command_cite_with_non_bib target) in
            Html.span ~a:[a_class (!!"cite" :: classnames)] (commafy ~prefix:"[" ~suffix:"]" (List.map link_maker pointers))

        | Dref (pointer, maybe_seq) ->
            let label = Label.Manual pointer in
            let target = Hashtbl.find labels label in
            let suffix = match maybe_seq with Some seq -> write_seq seq | None -> [] in
            let make_dref order = make_internal_link ~classnames label (Obj.magic (order @ suffix)) in
            begin match target with
                | Target.Visible (Target.Custom (_, Custom.Boxout, order)) ->
                    make_dref (boxout_conv order)
                | Target.Visible (Target.Custom (_, Custom.Theorem, order)) ->
                    make_dref (theorem_conv order)
                | Target.Visible (Target.Wrapper (_, order)) ->
                    make_dref (wrapper_conv order)
                | Target.Visible (Target.Part order) ->
                    make_dref (part_conv order)
                | Target.Visible (Target.Section (location, order)) ->
                    make_dref (section_conv location order)
                | _ ->
                    raise (Command_ref_with_non_visible_block target)
            end

        | Sref (pointer, maybe_seq) ->
            let label = Label.Manual pointer in
            let target = Hashtbl.find labels label in
            let suffix = match maybe_seq with Some seq -> write_seq seq | None -> [] in
            let make_sref wseq order = make_internal_link ~classnames label (Obj.magic (wseq @ order @ suffix)) in
            begin match target with
                | Target.Visible (Target.Custom (env, Custom.Boxout, order)) ->
                    make_sref (write_name (Name_custom env)) (boxout_conv ~prespace:true order)
                | Target.Visible (Target.Custom (env, Custom.Theorem, order)) ->
                    make_sref (write_name (Name_custom env)) (theorem_conv ~prespace:true order)
                | Target.Visible (Target.Wrapper (Wrapper.Equation, order)) ->
                    make_sref (write_name Name_equation) (wrapper_conv ~prespace:true order)
                | Target.Visible (Target.Wrapper (Wrapper.Printout, order)) ->
                    make_sref (write_name Name_printout) (wrapper_conv ~prespace:true order)
                | Target.Visible (Target.Wrapper (Wrapper.Table, order)) ->
                    make_sref (write_name Name_table) (wrapper_conv ~prespace:true order)
                | Target.Visible (Target.Wrapper (Wrapper.Figure, order)) ->
                    make_sref (write_name Name_figure) (wrapper_conv ~prespace:true order)
                | Target.Visible (Target.Part order) ->
                    make_sref (write_name Name_part) (part_conv ~prespace:true order)
                | Target.Visible (Target.Section (location, order)) ->
                    let name = match location with
                        | Mainbody   -> Name_section
                        | Appendixed -> Name_appendix in
                    make_sref (write_name name) (section_conv ~prespace:true location order)
                | _ ->
                    raise (Command_sref_with_non_visible_block target)
            end

        | Mref (pointer, seq) ->
            make_internal_link (Label.Manual pointer) (Obj.magic (write_seq seq))


    (****************************************************************************)
    (*  {4 Name writer}                                                         *)
    (****************************************************************************)

    and write_name =
        let cache = Hashtbl.create (Hashtbl.length customs) in
        fun name ->
            try Hashtbl.find cache name
            with Not_found ->
                let open Translations in
                let seq = match name with
                    | Name_custom env   -> Hashtbl.find customs env
                    | Name_equation     -> opts.translations.equation
                    | Name_printout     -> opts.translations.printout
                    | Name_table        -> opts.translations.table
                    | Name_figure       -> opts.translations.figure
                    | Name_part         -> opts.translations.part
                    | Name_appendix     -> opts.translations.appendix
                    | Name_section      -> opts.translations.section
                    | Name_bibliography -> opts.translations.bibliography
                    | Name_notes        -> opts.translations.notes
                    | Name_toc          -> opts.translations.toc
                    | Name_abstract     -> opts.translations.abstract in
                let value = write_seq seq in
                Hashtbl.add cache name value;
                value in


    (****************************************************************************)
    (*  {4 Writers for tabular environment}                                     *)
    (****************************************************************************)

    let write_tabular classnames tab =

        let open Tabular in

        let write_cell ord {attr; cellfmt; seq} =
            let (colfmt, maybe_colspan, overline, underline) = match (cellfmt, tab.tcols) with
                | (Some {colfmt; colspan; overline; underline}, _) -> (colfmt, Some colspan, overline, underline)
                | (None, Some tcols)                               -> (Array.get tcols ord, None, false, false)
                | (None, None)                                     -> ({alignment = Left; weight = Normal}, None, false, false) in
            let classnames = List.map (!!!) attr.classnames in
            let classnames = ("cell_" ^^ Tabular_output.string_of_alignment colfmt.alignment) :: classnames in
            let classnames = if overline then !!"oline" :: classnames else classnames in
            let classnames = if underline then !!"uline" :: classnames else classnames in
            let a_hd = a_class classnames in
            let a_tl = match maybe_colspan with Some n -> [a_colspan n] | None -> [] in
            let out_seq = match seq with Some seq -> write_seq seq | None -> [] in
            match colfmt.weight with
                | Normal -> Html.td ~a:(a_hd :: a_tl) (out_seq: Html_types.phrasing Html.elt list :> Html_types.td_content_fun Html.elt list)
                | Strong -> Html.th ~a:(a_hd :: a_tl) (out_seq: Html_types.phrasing Html.elt list :> Html_types.th_content_fun Html.elt list) in

        let write_row cells =
            Html.tr (List.mapi write_cell cells) in

        let write_group grp =
            List.map write_row grp in

        let thead = match tab.thead with
            | None     -> None
            | Some grp -> Some (Html.thead ~a:[a_class [!!"tgroup"]] (write_group grp)) in

        let tbodies =
            let write_tbody grp =
                Html.tbody ~a:[a_class [!!"tgroup"]] (write_group grp) in
            List.map write_tbody Tabular.(tab.tbodies) in

        let tfoot = match tab.tfoot with
            | None     -> None
            | Some grp -> Some (Html.tfoot ~a:[a_class [!!"tgroup"]] (write_group grp)) in

        Html.div ~a:[a_class (!!"tab" :: classnames)] [Html.div ~a:[a_class [!!"tab_aux"]]  [Html.tablex ?thead ?tfoot tbodies]] in


    (****************************************************************************)
    (*  {4 Writers for document blocks}                                         *)
    (****************************************************************************)

    let rec write_frag frag =
        List.flatten (List.map (write_block ~wrapped:false) frag)


    and write_block ?(wrapped = false) {blk; attr} =
        let classnames = List.map (!!!) attr.classnames in
        match blk with

        | Paragraph seq ->
            let extra =
                if opts.numbered_paragraphs
                then (incr paragraph_counter; [a_title (Translations.(opts.translations.paragraph) ^ " #" ^ (string_of_int !paragraph_counter))])
                else [] in
            [Html.p ~a:(a_class (!!"par" :: classnames) :: extra) (write_seq seq)]

        | Itemize frags ->
            let xs = List.map (fun frag -> Html.li ~a:[a_class [!!"item"]] (write_frag frag)) frags in
            [Html.ul ~a:[a_class (!!"itemize" :: classnames)] xs]

        | Enumerate frags ->
            let xs = List.map (fun frag -> Html.li ~a:[a_class [!!"item"]] (write_frag frag)) frags in
            [Html.ol ~a:[a_class (!!"enumerate" :: classnames)] xs]

        | Description elems ->
            let write_dfrag (seq, frag) accum =
                let dt = Html.dt ~a:[a_class [!!"item"]] (write_seq seq) in
                let dd = Html.dd ~a:[a_class [!!"item"]] (write_frag frag) in
                dt :: dd :: accum in
            [Html.dl ~a:[a_class (!!"description" :: classnames)] (List.fold_right write_dfrag elems [])]

        | Qanda elems ->
            let write_qfrag (qanda, frag) accum =
                let (qora, maybe_seq) = match qanda with
                    | Qanda.New_questioner maybe_seq -> last_question_seq := maybe_seq; (`Question, maybe_seq)
                    | Qanda.New_answerer maybe_seq   -> last_answer_seq := maybe_seq; (`Answer, maybe_seq)
                    | Qanda.Same_questioner      -> (`Question, !last_question_seq)
                    | Qanda.Same_answerer        -> (`Answer, !last_answer_seq) in
                let qora_class = match qora with
                    | `Question -> !!"question"
                    | `Answer   -> !!"answer" in
                let (outseq, empty_class) = match maybe_seq with
                    | Some seq -> (write_seq seq, [!!"empty"])
                    | None     -> ([], []) in
                let dt = Html.dt ~a:[a_class (qora_class :: empty_class)] outseq in
                let dd = Html.dd ~a:[a_class [qora_class]] (write_frag frag) in
                dt :: dd :: accum in
            [Html.dl ~a:[a_class (!!"qanda" :: classnames)] (List.fold_right write_qfrag elems [])]

        | Verse frag ->
            let aux = Html.div ~a:[a_class [!!"verse_aux"]] (write_frag frag) in
            [Html.div ~a:[a_class (!!"verse" :: classnames)] [aux]]

        | Quote frag ->
            [Html.blockquote ~a:[a_class (!!"quote" :: classnames)] (write_frag frag)]

        | Math_blk math ->
            let html: [> Html_types.div ] Html.elt = Html.Unsafe.data (Math_output.get_mathml math) in
            [Html.div ~a:[a_class (!!"math_blk" :: classnames)] [html]]

        | Source hilite ->
            [Hilite_writer.write_block ~class_prefix:!!"src_" ~extra_classes:classnames ~linenums:hilite.linenums hilite.data]

        | Tabular tab ->
            [write_tabular classnames tab]

        | Subpage frag ->
            [Html.div ~a:[a_class (!!"subpage" :: classnames)] (write_frag frag)]

        | Verbatim txt ->
            let aux = Html.div ~a:[a_class [!!"pre_aux"]] [Html.pre ~a:[a_class [!!"pre_aux"]] [Html.pcdata txt]] in
            [Html.div ~a:[a_class (!!"pre" :: classnames @ make_floatable wrapped)] [aux]]

        | Picture (href, alt, title, width) ->
            let suffix = match title with Some t -> [a_title t] | None -> [] in
            let wattr = match width with Some w -> [a_width w] | None -> [] in
            let uri = Html.uri_of_string href in
            let img = Html.a ~a:[a_href uri; a_class [!!"pic_lnk"]] [Html.img ~a:(a_class [!!"pic"] :: wattr) ~src:uri ~alt ()] in
            [Html.div ~a:(a_class (!!"pic" :: classnames @ make_floatable wrapped) :: suffix) [img]]

        | Pullquote (maybe_seq, frag) ->
            let head = match maybe_seq with
                | Some seq -> [Html.h1 ~a:[a_class [!!"pull_head"]] ([Html.entity "#x2014"; Html.entity "#x2002"] @ (write_seq seq))]
                | None     -> [] in
            let aux = Html.div ~a:[a_class [!!"pull_aux"]] ((write_frag frag) @ head) in
            [Html.div ~a:[a_class (!!"pull" :: classnames @ make_floatable false)] [aux]]

        | Boxout (data, maybe_seq, frag) ->
            let formatter = function
                | Some seq1, Some order, Some seq2 ->
                    seq1 @ (boxout_conv ~prespace:true order) @ [pcdata ":"; Html.entity "#xa0"] @ seq2
                | Some seq1, None, Some seq2 ->
                    seq1 @ [pcdata ":"; Html.entity "#xa0"] @ seq2
                | Some seq1, Some order, None ->
                    seq1 @ (boxout_conv ~prespace:true order)
                | Some seq1, None, None ->
                    seq1
                | None, None, Some seq2 ->
                    seq2
                | _ ->
                    [] in
            [write_custom data maybe_seq frag !!"boxout" (classnames @ make_floatable false) formatter]

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
                    | x  -> [Html.span ~a:[a_class [!!"thmname"]] x]
                and capbody = match bd with
                    | [] -> []
                    | x  -> [Html.span ~a:[a_class [!!"thmextra"]] x]
                in caphead @ capbody in
            [write_custom (data :> Custom.custom) maybe_seq frag !!"theorem" classnames formatter]

        | Equation (wrapper, blk) ->
            [write_wrapper wrapper blk !!"equation" classnames Name_equation]

        | Printout (wrapper, blk) ->
            [write_wrapper wrapper blk !!"printout" classnames Name_printout]

        | Table (wrapper, blk) ->
            [write_wrapper wrapper blk !!"table" classnames Name_table]

        | Figure (wrapper, blk) ->
            [write_wrapper wrapper blk !!"figure" classnames Name_figure]

        | Heading heading ->
            write_heading_block classnames heading

        | Autogen autogen ->
            write_autogen_block classnames autogen

        | Title (level, seq) ->
            [(cons_of_level (level :> int)) ~a:[a_class (!!"title" :: classnames)] (write_seq seq)]

        | Abstract frag ->
            let aux = Html.h1 ~a:[a_class [!!"sec"]] (write_name Name_abstract) :: (write_frag frag) in
            [Html.div ~a:[a_class (!!"abstract" :: classnames)] aux]

        | Rule ->
            [Html.hr ~a:[a_class (!!"rule" :: classnames)] ()]


    and write_heading_block classnames = function

        | Part (label, order, Custom_part seq) ->
            [make_heading Html.h1 label (part_conv ~spanify:true order) (!!"part" :: classnames) (write_seq seq)]

        | Part (label, order, Appendix) ->
            [make_heading Html.h1 label (part_conv ~spanify:true order) (!!"part" :: classnames) (write_name Name_appendix)]

        | Section (label, order, location, level, Custom_section seq) ->
            [make_sectional level label (section_conv ~spanify:true location order) classnames (write_seq seq)]

        | Section (label, order, location, level, Autogen_section Bibliography) ->
            let title = make_sectional level label (section_conv ~spanify:true location order) classnames (write_name Name_bibliography) in
            let bibs = write_bibs classnames in
            title :: bibs

        | Section (label, order, location, level, Autogen_section Notes) ->
            let title = make_sectional level label (section_conv ~spanify:true location order) classnames (write_name Name_notes) in
            let notes = write_notes classnames in
            title :: notes

        | Section (label, order, location, level, Autogen_section Toc) ->
            let title = make_sectional level label (section_conv ~spanify:true location order) classnames (write_name Name_toc) in
            let toc = write_toc classnames in
            title :: toc


    and write_autogen_block classnames = function
        | Bibliography -> write_bibs classnames
        | Notes        -> write_notes classnames
        | Toc          -> write_toc classnames


    and write_custom data maybe_seq frag classname classnames formatter =
        let (env, label, triple) = match data with
            | `Anonymous (env, label)       -> (env, label, (None, None, maybe write_seq maybe_seq))
            | `Unnumbered (env, label)      -> (env, label, (Some (write_name (Name_custom env)), None, maybe write_seq maybe_seq))
            | `Numbered (env, label, order) -> (env, label, (Some (write_name (Name_custom env)), Some order, maybe write_seq maybe_seq)) in
        let title = match formatter triple with
            | [] -> []
            | xs -> [Html.h1 ~a:[a_class [classname ^ "_head"]] xs] in
        let content = title @ [Html.div ~a:[a_class [classname ^ "_body"]] (write_frag frag)] in
        Html.div ~a:[a_id (make_label label); a_class (classname :: (classname ^ "_env_"  ^ env) :: classnames)] content


    and write_wrapper wrapper blk classname classnames name =
        let wrapper_content = match write_block ~wrapped:true blk with
            | [b] -> b
            | _   -> failwith "write_wrapper" in
        let (length, label, caption_content) = match wrapper with
            | Wrapper.Ordered (label, order, maybe_seq) ->
                let headcore = (write_name name) @ (wrapper_conv ~prespace:true order) in
                begin match maybe_seq with
                    | Some seq -> ("long", label, [Html.h1 ~a:[a_class [!!"caption_head"]] (headcore @ [pcdata ":"]); Html.p ~a:[a_class [!!"caption_body"]] (write_seq seq)])
                    | None     -> ("short", label, [Html.h1 ~a:[a_class [!!"caption_head"]] ([pcdata "("] @ headcore @ [pcdata ")"])])
                end
            | Wrapper.Unordered (label, seq) ->
                ("long", label, [Html.p ~a:[a_class [!!"caption_body"]] (write_seq seq)]) in
        let caption = Html.div ~a:[a_class [!!"caption"; "caption_" ^^ length]] [Html.div ~a:[a_class [!!"caption_aux"]] caption_content] in
        Html.div ~a:[a_id (make_label label); a_class (!!"wrapper" :: !!"floatable" :: classname :: classnames)] [wrapper_content; caption]


    (****************************************************************************)
    (*  {4 Writers for ghost elements}                                          *)
    (****************************************************************************)

    and write_bibs classnames = match bibs with
        | [] -> []
        | xs -> [Html.ol ~a:[a_class (!!"bibs" :: classnames)] (List.map write_bib xs)]


    and write_bib bib =
        let open Bib in
        Html.li ~a:[a_id (make_label bib.label); a_class [!!"bib"]]
            [
            Html.span ~a:[a_class [!!"bib_head"]] (pcdata "[" :: (bib_conv bib.order) @ [pcdata "]"]);
            Html.p ~a:[a_class [!!"bib_body"]]
                begin match bib.entry with
                    | Short seq -> 
                        [Html.span ~a:[a_class [!!"bib_short"]] (write_seq seq)]
                    | Long (author, title, resource) ->
                        [
                        Html.span ~a:[a_class [!!"bib_author"]] (write_seq author);
                        Html.span ~a:[a_class [!!"bib_title"]] (write_seq title);
                        Html.span ~a:[a_class [!!"bib_resource"]] (write_seq resource);
                        ]
                end
            ]


    and write_notes classnames = match notes with
        | [] -> []
        | xs -> [Html.ol ~a:[a_class (!!"notes" :: classnames)] (List.map write_note xs)]


    and write_note note =
        let open Note in
        Html.li ~a:[a_id (make_label note.label); a_class [!!"note"]]
            [
            Html.span ~a:[a_class [!!"note_head"]] (pcdata "(" :: (note_conv note.order) @ [pcdata ")"]);
            Html.div ~a:[a_class [!!"note_body"]] (write_frag note.content);
            ]


    and write_toc classnames = match List.filter_map write_toc_entry toc with
        | [] -> []
        | xs -> [Html.ul ~a:[a_class (!!"toc" :: classnames)] xs]


    and write_toc_entry sec =
        let make_toc_entry label classname orderlst content =
            Some (Html.li ~a:[a_class [!!"item"; classname]] [make_internal_link label (orderlst @ content)])
        in match sec with
            | Part (label, order, Custom_part seq) ->
                make_toc_entry label (class_of_level 0) (part_conv ~spanify:true order) (Obj.magic (write_seq seq))
            | Part (label, order, Appendix) ->
                make_toc_entry label (class_of_level 0) (part_conv ~spanify:true order) (Obj.magic (write_name Name_appendix))
            | Section (label, order, location, level, Custom_section seq) ->
                make_toc_entry label (class_of_level (level :> int)) (section_conv ~spanify:true location order) (Obj.magic (write_seq seq))
            | Section (label, order, location, level, Autogen_section Bibliography) ->
                make_toc_entry label (class_of_level (level :> int)) (section_conv ~spanify:true location order) (Obj.magic (write_name Name_bibliography))
            | Section (label, order, location, level, Autogen_section Notes) ->
                make_toc_entry label (class_of_level (level :> int)) (section_conv ~spanify:true location order) (Obj.magic (write_name Name_notes))
            | Section (label, order, location, level, Autogen_section Toc) ->
                make_toc_entry label (class_of_level (level :> int)) (section_conv ~spanify:true location order) (Obj.magic (write_name Name_toc))


    in Html.div ~a:[a_class (opts.prefix :: (List.map (!!) opts.base_classes) @ opts.extra_classes)] (write_frag content)


(********************************************************************************)
(** {3 Conversion of invalid documents}                                         *)
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
                    Html.li ~a:[a_class [classname]]
                        [
                        Html.span ~a:[a_class [!!"error_linenum"]] [pcdata (sprintf "%03d" (line_number + delta))];
                        Html.span ~a:[a_class [!!"error_linestr"]] [pcdata line]
                        ] in
                let show_line_around delta line =
                    show_line !!"error_around" delta line
                and show_line_actual =
                    show_line !!"error_actual" 0 line_actual in
                let lines =
                    (List.mapi (fun ord line -> show_line_around (ord - (List.length line_before)) line) line_before) @
                    [show_line_actual] @
                    (List.mapi (fun ord line -> show_line_around (ord+1) line) line_after)
                in  [
                    Html.h1 ~a:[a_class [!!"error_head"]] [pcdata (sprintf "Error in line %d:" line_number)];
                    Html.ul ~a:[a_class [!!"error_lines"]] (List.hd lines :: List.tl lines);
                    ]
            | None ->
                [Html.h1 ~a:[a_class [!!"error_head"]] [pcdata "Global error:"]] in
        let explanation_doc = Valid.make [Block.paragraph (Explanations.explain error)] in
        let valid_options = {default_valid_options with prefix = opts.prefix; base_classes = ["error_msg"]} in
        let explanation_out = from_valid ~valid_options explanation_doc in
        Html.li ~a:[a_class [!!"error"]] (context @ [explanation_out]) in
    let errors = List.map write_error doc in
    Html.div ~a:[a_class (opts.prefix :: (List.map (!!) opts.base_classes) @ (List.map (!!) opts.extra_classes))] [ul ~a:[a_class [!!"errors"]] errors]
end

