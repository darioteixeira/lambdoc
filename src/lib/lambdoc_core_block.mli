(********************************************************************************)
(*  Lambdoc_core_block.mli
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Definitions concerning block elements.
*)

module Attr = Lambdoc_core_attr
module Basic = Lambdoc_core_basic
module Custom = Lambdoc_core_custom
module Heading = Lambdoc_core_heading
module Inline = Lambdoc_core_inline
module Level = Lambdoc_core_level
module Math = Lambdoc_core_math
module Qanda = Lambdoc_core_qanda
module Hilite = Lambdoc_core_hilite
module Tabular = Lambdoc_core_tabular
module Wrapper = Lambdoc_core_wrapper

open Basic


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

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
    | Boxout of Custom.Boxout.t * Inline.seq option * frag
    | Theorem of Custom.Theorem.t * Inline.seq option * frag
    | Equation of Wrapper.t * t
    | Printout of Wrapper.t * t
    | Table of Wrapper.t * t
    | Figure of Wrapper.t * t
    | Heading of Heading.t
    | Title of Level.title * Inline.seq
    | Abstract of frag
    | Rule

and t =
    {
    blk: block;
    attr: Attr.t;
    }

and frag = t list [@@deriving sexp]


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

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
val boxout:      ?attr:Attr.t -> Custom.Boxout.t -> Inline.seq option -> frag -> t
val theorem:     ?attr:Attr.t -> Custom.Theorem.t -> Inline.seq option -> frag -> t
val equation:    ?attr:Attr.t -> Wrapper.t -> t -> t
val printout:    ?attr:Attr.t -> Wrapper.t -> t -> t
val table:       ?attr:Attr.t -> Wrapper.t -> t -> t
val figure:      ?attr:Attr.t -> Wrapper.t -> t -> t
val heading:     ?attr:Attr.t -> Heading.t -> t
val title:       ?attr:Attr.t -> Level.title -> Inline.seq -> t
val abstract:    ?attr:Attr.t -> frag -> t
val rule:        ?attr:Attr.t -> unit -> t

