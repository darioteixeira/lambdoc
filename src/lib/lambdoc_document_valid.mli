(** Definition of a valid Lambdoc document.
*)


(********************************************************************************)
(** {1 Basic data types}														*)
(********************************************************************************)

type entity = string [@@deriving sexp]      (** HTML entities *)
type href = string [@@deriving sexp]        (** References to external resources *)
type classname = string [@@deriving sexp]   (** CSS classnames *)
type ident = string [@@deriving sexp]       (** Identifiers *)
type pointer = string [@@deriving sexp]     (** Internal references *)


(********************************************************************************)
(** {1 Attributes}                                                              *)
(********************************************************************************)

module Attr:
sig
    type origin =
        | Source        (** Straight from the source as provided by the user *)
        | Extension     (** Synthesised from an extension *)
        [@@deriving sexp]

    type parsinfo =
        {
        tag: ident option;
        linenum: int;
        origin: origin;
        } [@@deriving sexp, make]

    type t =
        {
        classnames: classname list;
        parsinfo: parsinfo option;
        } [@@deriving sexp, make]

    val default: t
end


(********************************************************************************)
(** {1 Mathematics}																*)
(********************************************************************************)

module Math:
sig
    type t =
        | Mathtex of string
        | Mathml of string
        | Both of string * string	(* (mathtex, mathml) *)
        [@@deriving sexp]
end


(********************************************************************************)
(** {1 Highlighted source-code}													*)
(********************************************************************************)

module Hilite:
sig
    type t =
        {
        lang: Camlhighlight_core.lang option;
        linenums: bool;
        data: Camlhighlight_core.t [@main];
        } [@@deriving sexp, make]
end


(********************************************************************************)
(** {1 Inline elements}															*)
(********************************************************************************)

module Inline:
sig
    type inline =
        | Plain of string                           (** Plain, unadorned text *)
        | Entity of entity                          (** Unicode character entity *)
        | Linebreak                                 (** Line break within same paragraph *)
        | Math_inl of Math.t                        (** Inline mathematics *)
        | Code of Hilite.t                          (** Inline highlighted source-code *)
        | Glyph of href * string * string option    (** Inline image with href, alternative text, and optional title *)
        | Bold of seq                               (** Bold text *)
        | Emph of seq                               (** Emphasised text (italic) *)
        | Mono of seq                               (** Monospaced sequence *)
        | Caps of seq                               (** All-caps text *)
        | Ins of seq                                (** Text replacing wrong text *)
        | Del of seq                                (** Text to be replaced (strike-through) *)
        | Sup of seq                                (** Superscript *)
        | Sub of seq                                (** Subscript *)
        | Mbox of seq                               (** Text sequence which should not be broken across lines *)
        | Span of seq                               (** A custom span of text *)
        | Link of href * seq option                 (** Reference to an external resource *)
        | See of pointer list                       (** Reference to an end note *)
        | Cite of pointer list                      (** Citation of a bibliography entry *)
        | Dref of pointer * seq option              (** Dumb reference to an internal element *)
        | Sref of pointer * seq option              (** Smart reference to an internal element *)
        | Mref of pointer * seq                     (** Manual reference to an internal element *)

    and t =
        {
        inl: inline; 
        attr: Attr.t;
        }

    and seq = t list [@@deriving sexp]

    val plain:     ?attr:Attr.t -> string -> t
    val entity:    ?attr:Attr.t -> entity -> t
    val linebreak: ?attr:Attr.t -> unit -> t
    val math_inl:  ?attr:Attr.t -> Math.t -> t
    val code:      ?attr:Attr.t -> Hilite.t -> t
    val glyph:     ?attr:Attr.t -> href -> string -> string option -> t
    val bold:      ?attr:Attr.t -> seq -> t
    val emph:      ?attr:Attr.t -> seq -> t
    val mono:      ?attr:Attr.t -> seq -> t
    val caps:      ?attr:Attr.t -> seq -> t
    val ins:       ?attr:Attr.t -> seq -> t
    val del:       ?attr:Attr.t -> seq -> t
    val sup:       ?attr:Attr.t -> seq -> t
    val sub:       ?attr:Attr.t -> seq -> t
    val mbox:      ?attr:Attr.t -> seq -> t
    val span:      ?attr:Attr.t -> seq -> t
    val link:      ?attr:Attr.t -> href -> seq option -> t
    val see:       ?attr:Attr.t -> pointer list -> t
    val cite:      ?attr:Attr.t -> pointer list -> t
    val dref:      ?attr:Attr.t -> pointer -> seq option -> t
    val sref:      ?attr:Attr.t -> pointer -> seq option -> t
    val mref:      ?attr:Attr.t -> pointer -> seq -> t
end


(********************************************************************************)
(** {1 Automatically-generated content}											*)
(********************************************************************************)

module Autogen:
sig
    type t =
        | Bibliography
        | Notes
        | Toc
        [@@deriving sexp]
end


(********************************************************************************)
(** {1 Labels}																	*)
(********************************************************************************)

module Label:
sig
    type t =
        | Auto of pointer	    (** Automatically created by the system *)
        | Manual of pointer	    (** Manually specified in the source *)
        [@@deriving sexp]
end


(********************************************************************************)
(** {1 Ordering}																*)
(********************************************************************************)

(** A block's ordering can be assigned by any of three sources: [`Auto_given]
	means that the ordering should be automatically given by the system;
	[`User_given] means that the ordering is manually given by the user;
	finally, when the block should not have any ordering at all, [`None_given]
	is used.  Note that different classes of blocks allow a different subset
	of these ordering variants.  Moreover, only the first two variants must be
	parametrised over the actual ordering scheme used (as it makes no sense to
	talk of an ordering scheme when [`None_given] is used, for example).
*)
module Order:
sig
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
(** {1 Custom blocks}															*)
(********************************************************************************)

module Custom:
sig
    type kind = Boxout | Theorem [@@deriving sexp]
    type key = pointer [@@deriving sexp]
    type order = Order.ordinal Order.full [@@deriving sexp]

    type anonymous = [ `Anonymous of key * Label.t ] [@@deriving sexp]
    type unnumbered = [ `Unnumbered of key * Label.t ] [@@deriving sexp]
    type numbered = [ `Numbered of key * Label.t * order ] [@@deriving sexp]

    type custom = [ anonymous | unnumbered | numbered ]
    type boxout = [ anonymous | unnumbered | numbered ] [@@deriving sexp]
    type theorem = [ unnumbered | numbered ] [@@deriving sexp]

    val anonymous: key -> Label.t -> order -> [> anonymous ]
    val unnumbered: key -> Label.t -> order -> [> unnumbered ]
    val numbered: key -> Label.t -> order -> [> numbered ]

    val boxout: [ anonymous | unnumbered | numbered ] -> boxout
    val theorem: [ anonymous | unnumbered | numbered ] -> theorem
end


(********************************************************************************)
(** {1 Bibliography blocks}														*)
(********************************************************************************)

module Bib:
sig
    type order = (Order.ordinal, Order.ordinal Order.auto_given) Order.t [@@deriving sexp]

    type entry =
        | Short of Inline.seq
        | Long of Inline.seq * Inline.seq * Inline.seq  (* (author, title, resource) *) 
        [@@deriving sexp]

    type t =
        {
        label: Label.t;
        order: order;
        entry: entry;
        } [@@deriving sexp, make]
end


(********************************************************************************)
(** {1 Hierarchy levels}                                                        *)
(********************************************************************************)

module Level:
sig
    (** Definition of hierarchy levels for sections.  We support a
        six-level hierarchy, equivalent to HTML's H1 to H6.
    *)
    type section = private int [@@deriving sexp]

    (** Definition of hierarchy levels for titles.  We support a
        two-level hierarchy, equivalent to HTML's H1 and H2.
        These can be interpreted as "title" and "subtitle".
    *)
    type title = private int [@@deriving sexp]

    (** Maximum accepted hierarchical level.
    *)
    val max_section: int

    (** Maximum accepted title level.
    *)
    val max_title: int

    (** Constructor for {!section}.  We force the use
        of this constructor by making the type private.
    *)
    val section: int -> section

    (** Constructor for {!title}.  We force the use
        of this constructor by making the type private.
    *)
    val title: int -> title
end


(********************************************************************************)
(** {1 Headings}																*)
(********************************************************************************)

module Heading:
sig
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

    val part: Label.t -> part_order -> Inline.seq -> t
    val appendix: Label.t -> t
    val section: Label.t -> section_order -> section_location -> Level.section -> Inline.seq -> t
    val bibliography: Label.t -> t
    val notes: Label.t -> t
    val toc: Label.t -> t
end


(********************************************************************************)
(** {1 Q&A blocks}																*)
(********************************************************************************)

module Qanda:
sig
    type t =
        | New_questioner of Inline.seq option
        | New_answerer of Inline.seq option
        | Same_questioner
        | Same_answerer
        [@@deriving sexp]
end


(********************************************************************************)
(** {1 Tabular}																	*)
(********************************************************************************)

module Tabular:
sig
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
        } [@@deriving sexp, make]

    type cellfmt =
        {
        colfmt: colfmt;
        colspan: int;
        overline: bool;
        underline: bool;
        } [@@deriving sexp, make]

    type cell =
        {
        attr: Attr.t [@default Attr.default];
        cellfmt: cellfmt option;
        seq: Inline.seq option [@main];
        } [@@deriving sexp, make]

    type row = cell list [@@deriving sexp]

    type group = row list [@@deriving sexp]

    type t =
        {
        tcols: colfmt array option;
        thead: group option;
        tfoot: group option;
        tbodies: group list [@main];
        } [@@deriving sexp, make]
end


(********************************************************************************)
(** {1 Wrapper blocks}                                                        	*)
(********************************************************************************)

module Wrapper:
sig
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
(** {1 Block elements}                                                        	*)
(********************************************************************************)

module Block:
sig
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

    val paragraph:   ?attr:Attr.t -> Inline.seq -> t
    val itemize:     ?attr:Attr.t -> frag list -> t
    val enumerate:   ?attr:Attr.t -> frag list -> t
    val description: ?attr:Attr.t -> (Inline.seq * frag) list -> t
    val qanda:       ?attr:Attr.t -> (Qanda.t * frag) list -> t
    val verse:       ?attr:Attr.t -> frag -> t
    val quote:       ?attr:Attr.t -> frag -> t
    val math_blk:    ?attr:Attr.t -> Math.t -> t
    val source:      ?attr:Attr.t -> Hilite.t -> t
    val tabular:     ?attr:Attr.t -> Tabular.t -> t
    val subpage:     ?attr:Attr.t -> frag -> t
    val verbatim:    ?attr:Attr.t -> string -> t
    val picture:     ?attr:Attr.t -> href -> string -> string option -> int option -> t
    val pullquote:   ?attr:Attr.t -> Inline.seq option -> frag -> t
    val boxout:      ?attr:Attr.t -> Custom.boxout -> Inline.seq option -> frag -> t
    val theorem:     ?attr:Attr.t -> Custom.theorem -> Inline.seq option -> frag -> t
    val equation:    ?attr:Attr.t -> Wrapper.t -> t -> t
    val printout:    ?attr:Attr.t -> Wrapper.t -> t -> t
    val table:       ?attr:Attr.t -> Wrapper.t -> t -> t
    val figure:      ?attr:Attr.t -> Wrapper.t -> t -> t
    val heading:     ?attr:Attr.t -> Heading.t -> t
    val autogen:     ?attr:Attr.t -> Autogen.t -> t
    val title:       ?attr:Attr.t -> Level.title -> Inline.seq -> t
    val abstract:    ?attr:Attr.t -> frag -> t
    val rule:        ?attr:Attr.t -> unit -> t
end


(********************************************************************************)
(** {1 Note blocks}                                                        		*)
(********************************************************************************)

module Note:
sig
    type order = (Order.ordinal, Order.ordinal Order.auto_given) Order.t [@@deriving sexp]

    type t =
        {
        label: Label.t;
        order: order;
        content: Block.frag;
        } [@@deriving sexp, make]
end


(********************************************************************************)
(** {1 Targets}                                                        			*)
(********************************************************************************)

module Target:
sig
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

	val custom: Custom.key -> Custom.kind -> Custom.order -> t
	val wrapper: Wrapper.kind -> Wrapper.order -> t
	val part: Heading.part_order -> t
	val section: Heading.section_location -> Heading.section_order -> t
	val bib: Bib.order -> t
	val note: Note.order -> t
end


(********************************************************************************)
(** {1 Valid documents}                                                    		*)
(********************************************************************************)

type labels = (Label.t, Target.t) Hashtbl.t [@@deriving sexp]
type customs = (Custom.key, Inline.seq) Hashtbl.t [@@deriving sexp]

type t =
	{
	bibs: Bib.t list;
	notes: Note.t list;
	toc: Heading.t list;
    labels: labels [@default Hashtbl.create 0];
    customs: customs [@default Hashtbl.create 0];
    content: Block.frag [@main];
	} [@@deriving sexp, make]

val serialize: t -> string
val deserialize: string -> t

