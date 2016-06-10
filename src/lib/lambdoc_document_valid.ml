(********************************************************************************)
(*  Lambdoc_document_valid.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Sexplib.Std


(********************************************************************************)
(** {1 Basic data types}                                                        *)
(********************************************************************************)

type entity = string [@@deriving sexp]
type href = string [@@deriving sexp]
type classname = string [@@deriving sexp]
type ident = string [@@deriving sexp]
type pointer = string [@@deriving sexp]


(********************************************************************************)
(** {1 Attributes}                                                              *)
(********************************************************************************)

module Attr =
struct
    type origin =
        | Source
        | Extension
        [@@deriving sexp]

    type parsinfo =
        {
        tag: ident option;
        linenum: int;
        origin: origin;
        } [@@deriving sexp]

    type t =
        {
        classnames: classname list;
        parsinfo: parsinfo option;
        } [@@deriving sexp]

    let make_parsinfo ?tag ~linenum ~origin () =
        {tag; linenum; origin}

    let make ?(classnames = []) ?parsinfo () =
        {classnames; parsinfo}

    let default = make ()
end


(********************************************************************************)
(** {1 Mathematics}                                                             *)
(********************************************************************************)

module Math =
struct
    type t =
        | Mathtex of string
        | Mathml of string
        | Both of string * string
        [@@deriving sexp]
end


(********************************************************************************)
(** {1 Highlighted source-code}                                                 *)
(********************************************************************************)

module Hilite =
struct
    type t =
        {
        lang: Camlhighlight_core.lang option;
        linenums: bool;
        data: Camlhighlight_core.t;
        } [@@deriving sexp]

    let make ?lang ~linenums data =
        {lang; linenums; data}
end


(********************************************************************************)
(** {1 Inline elements}                                                         *)
(********************************************************************************)

module Inline =
struct
    type inline =
        | Plain of string
        | Entity of entity
        | Linebreak
        | Math_inl of Math.t
        | Code of Hilite.t
        | Glyph of href * string * string option
        | Bold of seq
        | Emph of seq
        | Mono of seq
        | Caps of seq
        | Ins of seq
        | Del of seq
        | Sup of seq
        | Sub of seq
        | Mbox of seq
        | Span of seq
        | Link of href * seq option
        | See of pointer list
        | Cite of pointer list
        | Dref of pointer * seq option
        | Sref of pointer * seq option
        | Mref of pointer * seq

    and t =
        {
        inl: inline;
        attr: Attr.t;
        }

    and seq = t list [@@deriving sexp]

    let plain     ?(attr = Attr.default) txt = {inl = Plain txt; attr}
    let entity    ?(attr = Attr.default) ent = {inl = Entity ent; attr}
    let linebreak ?(attr = Attr.default) () = {inl = Linebreak; attr}
    let math_inl  ?(attr = Attr.default) data = {inl = Math_inl data; attr}
    let code      ?(attr = Attr.default) data = {inl = Code data; attr}
    let glyph     ?(attr = Attr.default) href alt title = {inl = Glyph (href, alt, title); attr}
    let bold      ?(attr = Attr.default) seq = {inl = Bold seq; attr}
    let emph      ?(attr = Attr.default) seq = {inl = Emph seq; attr}
    let mono      ?(attr = Attr.default) seq = {inl = Mono seq; attr}
    let caps      ?(attr = Attr.default) seq = {inl = Caps seq; attr}
    let ins       ?(attr = Attr.default) seq = {inl = Ins seq; attr}
    let del       ?(attr = Attr.default) seq = {inl = Del seq; attr}
    let sup       ?(attr = Attr.default) seq = {inl = Sup seq; attr}
    let sub       ?(attr = Attr.default) seq = {inl = Sub seq; attr}
    let mbox      ?(attr = Attr.default) seq = {inl = Mbox seq; attr}
    let span      ?(attr = Attr.default) seq = {inl = Span seq; attr}
    let link      ?(attr = Attr.default) href maybe_seq = {inl = Link (href, maybe_seq); attr}
    let see       ?(attr = Attr.default) pointers = {inl = See pointers; attr}
    let cite      ?(attr = Attr.default) pointers = {inl = Cite pointers; attr}
    let dref      ?(attr = Attr.default) pointer maybe_seq = {inl = Dref (pointer, maybe_seq); attr}
    let sref      ?(attr = Attr.default) pointer maybe_seq = {inl = Sref (pointer, maybe_seq); attr}
    let mref      ?(attr = Attr.default) pointer seq = {inl = Mref (pointer, seq); attr}
end


(********************************************************************************)
(** {1 Automatically-generated content}                                         *)
(********************************************************************************)

module Autogen =
struct
	type t =
		| Bibliography
		| Notes
		| Toc
		[@@deriving sexp]
end


(********************************************************************************)
(** {1 Labels}                                                                  *)
(********************************************************************************)

module Label =
struct
	type t =
		| Auto of pointer
		| Manual of pointer
		[@@deriving sexp]
end


(********************************************************************************)
(** {1 Ordering}                                                                *)
(********************************************************************************)

module Order =
struct
	type ordinal = int [@@deriving sexp]
	type hierarchical = int list [@@deriving sexp]

	type 'a auto_given = [ `Auto_given of 'a ] [@@deriving sexp]
	type 'a user_given = [ `User_given of 'a ] [@@deriving sexp]
	type none_given = [ `None_given ] [@@deriving sexp]

	type ('a, 'b) t = 'b constraint 'b = [< 'a auto_given | 'a user_given | none_given ] [@@deriving sexp]
	type 'a full = ('a, [ 'a auto_given | 'a user_given | none_given ]) t [@@deriving sexp]
	type 'a mandatory = ('a, [ 'a auto_given | 'a user_given ]) t [@@deriving sexp]
end


(********************************************************************************)
(** {1 Custom blocks}                                                           *)
(********************************************************************************)

module Custom =
struct
    type kind = Boxout | Theorem [@@deriving sexp]
    type key = pointer [@@deriving sexp]
    type order = Order.ordinal Order.full [@@deriving sexp]

    type anonymous = [ `Anonymous of key * Label.t ] [@@deriving sexp]
    type unnumbered = [ `Unnumbered of key * Label.t ] [@@deriving sexp]
    type numbered = [ `Numbered of key * Label.t * order ] [@@deriving sexp]

    type custom = [ anonymous | unnumbered | numbered ]
    type boxout = [ anonymous | unnumbered | numbered ] [@@deriving sexp]
    type theorem = [ unnumbered | numbered ] [@@deriving sexp]

	let anonymous key label = function
		| `None_given -> `Anonymous (key, label)
		| _           -> invalid_arg "Lambdoc_document_valid.Custom.anonymous"

	let unnumbered key label = function
		| `None_given -> `Unnumbered (key, label)
		| _           -> invalid_arg "Lambdoc_document_valid.Custom.unnumbered"

	let numbered key label order =
		`Numbered (key, label, order)

	let boxout x = x

	let theorem = function
		| #unnumbered | #numbered as x -> x
		| `Anonymous _                 -> invalid_arg "Lambdoc_document_valid.Custom.theorem"
end


(********************************************************************************)
(** {1 Bibliography blocks}                                                     *)
(********************************************************************************)

module Bib =
struct
    type order = (Order.ordinal, Order.ordinal Order.auto_given) Order.t [@@deriving sexp]

    type entry =
        | Short of Inline.seq
        | Long of Inline.seq * Inline.seq * Inline.seq
        [@@deriving sexp]

    type t =
        {
        label: Label.t;
        order: order;
        entry: entry;
        } [@@deriving sexp]

	let make ~label ~order ~entry =
		{label; order; entry}
end

(********************************************************************************)
(** {1 Hierarchy levels}                                                     	*)
(********************************************************************************)

module Level =
struct
	type section = int [@@deriving sexp]

	type title = int [@@deriving sexp]

	let max_section = 6

	let max_title = 2

	let section level =
		if level >= 1 && level <= max_section
		then level
		else invalid_arg ("Lambdoc_document_valid.Level.section: " ^ string_of_int level)

	let title level =
		if level >= 1 && level <= max_title
		then level
		else invalid_arg ("Lambdoc_document_valid.Level.title: " ^ string_of_int level)
end


(********************************************************************************)
(** {1 Headings}                                                                *)
(********************************************************************************)

module Heading =
struct
    type part_order = Order.ordinal Order.full [@@deriving sexp]
    type section_order = Order.hierarchical Order.full [@@deriving sexp]

    type part_content =
        | Custom_part of Inline.seq
        | Appendix
        [@@deriving sexp]

    type section_content =
        | Custom_section of Inline.seq
        | Autogen_section of Autogen.t
        [@@deriving sexp]

    type section_location =
        | Mainbody
        | Appendixed
        [@@deriving sexp]

    type t =
        | Part of Label.t * part_order * part_content
        | Section of Label.t * section_order * section_location * Level.section * section_content
        [@@deriving sexp]

	let part label order seq = Part (label, order, Custom_part seq)
	let appendix label = Part (label, `None_given, Appendix)
	let section label order location level seq = Section (label, order, location, level, Custom_section seq)
	let bibliography label = Section (label, `None_given, Mainbody, Level.section 1, Autogen_section Bibliography)
	let notes label = Section (label, `None_given, Mainbody, Level.section 1, Autogen_section Notes)
	let toc label = Section (label, `None_given, Mainbody, Level.section 1, Autogen_section Toc)
end


(********************************************************************************)
(** {1 Q&A blocks}                                                              *)
(********************************************************************************)

module Qanda =
struct
	type t =
		| New_questioner of Inline.seq option
		| New_answerer of Inline.seq option
		| Same_questioner
		| Same_answerer
		[@@deriving sexp]
end


(********************************************************************************)
(** {1 Tabular}                                                                 *)
(********************************************************************************)

module Tabular =
struct
	type alignment =
		| Center
		| Left
		| Right
		| Justify
		[@@deriving sexp]

	type weight =
		| Normal
		| Strong
		[@@deriving sexp]

	type colfmt =
		{
		alignment: alignment; 
		weight: weight; 
		} [@@deriving sexp]

	type cellfmt =
		{
		colfmt: colfmt;
		colspan: int;
		overline: bool;
		underline: bool;
		} [@@deriving sexp]

	type cell =
		{
		attr: Attr.t;
		cellfmt: cellfmt option;
		seq: Inline.seq option;
		} [@@deriving sexp]

	type row = cell list [@@deriving sexp]

	type group = row list [@@deriving sexp]

	type t =
		{
		tcols: colfmt array option;
		thead: group option;
		tfoot: group option;
		tbodies: group list;
		} [@@deriving sexp]

    let make_colfmt ~alignment ~weight = {alignment; weight}
	let make_cellfmt ~colfmt ~colspan ~overline ~underline = {colfmt; colspan; overline; underline}
	let make_cell ?(attr = Attr.default) ?cellfmt seq = {attr; cellfmt; seq}
	let make ?tcols ?thead ?tfoot tbodies = {tcols; thead; tfoot; tbodies}
end


(********************************************************************************)
(** {1 Wrapper blocks}                                                          *)
(********************************************************************************)

module Wrapper =
struct
    type order = Order.ordinal Order.full [@@deriving sexp]

    type kind =
        | Printout
        | Equation
        | Figure
        | Table
        [@@deriving sexp]

    type t =
        | Ordered of Label.t * Order.ordinal Order.mandatory * Inline.seq option
        | Unordered of Label.t * Inline.seq
        [@@deriving sexp]
end


(********************************************************************************)
(** {1 Block elements}                                                          *)
(********************************************************************************)

module Block =
struct
    type block =
        | Paragraph of Inline.seq
        | Itemize of frag list
        | Enumerate of frag list
        | Description of (Inline.seq * frag) list
        | Qanda of (Qanda.t * frag) list
        | Verse of frag
        | Quote of frag
        | Math_blk of Math.t
        | Source of Hilite.t
        | Tabular of Tabular.t
        | Subpage of frag
        | Verbatim of string
        | Picture of href * string * string option * int option
        | Pullquote of Inline.seq option * frag
        | Boxout of Custom.boxout * Inline.seq option * frag
        | Theorem of Custom.theorem * Inline.seq option * frag
        | Equation of Wrapper.t * t
        | Printout of Wrapper.t * t
        | Table of Wrapper.t * t
        | Figure of Wrapper.t * t
        | Heading of Heading.t
        | Autogen of Autogen.t
        | Title of Level.title * Inline.seq
        | Abstract of frag
        | Rule

    and t =
        {
        blk: block;
        attr: Attr.t;
        }

    and frag = t list [@@deriving sexp]

	let paragraph   ?(attr = Attr.default) seq = {blk = Paragraph seq; attr}
	let itemize     ?(attr = Attr.default) frags = {blk = Itemize frags; attr}
	let enumerate   ?(attr = Attr.default) frags = {blk = Enumerate frags; attr}
	let description ?(attr = Attr.default) dfrags = {blk = Description dfrags; attr}
	let qanda       ?(attr = Attr.default) qafrags = {blk = Qanda qafrags; attr}
	let verse       ?(attr = Attr.default) frag = {blk = Verse frag; attr}
	let quote       ?(attr = Attr.default) frag = {blk = Quote frag; attr}
	let math_blk    ?(attr = Attr.default) data = {blk = Math_blk data; attr}
	let source      ?(attr = Attr.default) data = {blk = Source data; attr}
	let tabular     ?(attr = Attr.default) data = {blk = Tabular data; attr}
	let subpage     ?(attr = Attr.default) frag = {blk = Subpage frag; attr}
	let verbatim    ?(attr = Attr.default) txt = {blk = Verbatim txt; attr}
	let picture     ?(attr = Attr.default) href alt title width = {blk = Picture (href, alt, title, width); attr}
	let pullquote   ?(attr = Attr.default) maybe_seq frag = {blk = Pullquote (maybe_seq, frag); attr}
	let boxout      ?(attr = Attr.default) data maybe_seq frag = {blk = Boxout (data, maybe_seq, frag); attr}
	let theorem     ?(attr = Attr.default) data maybe_seq frag = {blk = Theorem (data, maybe_seq, frag); attr}
	let equation    ?(attr = Attr.default) wrapper blk = {blk = Equation (wrapper, blk); attr}
	let printout    ?(attr = Attr.default) wrapper blk = {blk = Printout (wrapper, blk); attr}
	let table       ?(attr = Attr.default) wrapper blk = {blk = Table (wrapper, blk); attr}
	let figure      ?(attr = Attr.default) wrapper blk = {blk = Figure (wrapper, blk); attr}
	let heading     ?(attr = Attr.default) data = {blk = Heading data; attr}
	let autogen     ?(attr = Attr.default) data = {blk = Autogen data; attr}
	let title       ?(attr = Attr.default) level seq = {blk = Title (level, seq); attr}
	let abstract    ?(attr = Attr.default) frag = {blk = Abstract frag; attr}
	let rule        ?(attr = Attr.default) () = {blk = Rule; attr}
end

(********************************************************************************)
(** {1 Note blocks}                                                             *)
(********************************************************************************)

module Note =
struct
    type order = (Order.ordinal, Order.ordinal Order.auto_given) Order.t [@@deriving sexp]

    type t =
        {
        label: Label.t;
        order: order;
        content: Block.frag;
        } [@@deriving sexp]

	let make ~label ~order ~content =
		{label; order; content}
end


(********************************************************************************)
(** {1 Targets}                                                                 *)
(********************************************************************************)

module Target =
struct
    type visible =
        | Custom of Custom.key * Custom.kind * Custom.order
        | Wrapper of Wrapper.kind * Wrapper.order
        | Part of Heading.part_order
        | Section of Heading.section_location * Heading.section_order
        [@@deriving sexp]

    type t =
        | Visible of visible
        | Bib of Bib.order
        | Note of Note.order
        [@@deriving sexp]

	let custom env kind order = Visible (Custom (env, kind, order))
	let wrapper kind order = Visible (Wrapper (kind, order))
	let part order = Visible (Part order)
	let section location order = Visible (Section (location, order))
	let bib order = Bib order
	let note order = Note order
end


(********************************************************************************)
(** {1 Valid documents}                                                         *)
(********************************************************************************)

type labels = (Label.t, Target.t) Hashtbl.t [@@deriving sexp]
type customs = (Custom.key, Inline.seq) Hashtbl.t [@@deriving sexp]

type t =
    {
    bibs: Bib.t list;
    notes: Note.t list;
    toc: Heading.t list;
    labels: labels;
    customs: customs;
    content: Block.frag;
    } [@@deriving sexp]

let make ?(bibs = []) ?(notes = []) ?(toc = []) ?(labels = Hashtbl.create 0) ?(customs = Hashtbl.create 0) content =
    {bibs; notes; toc; labels; customs; content}

let serialize doc =
    Sexplib.Sexp.to_string_mach (sexp_of_t doc)

let deserialize str =
    t_of_sexp (Sexplib.Sexp.of_string str)

