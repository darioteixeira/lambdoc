(********************************************************************************)
(*  Lambdoc_core_inline.mli
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Definitions concerning inline elements.
*)

module Attr = Lambdoc_core_attr
module Basic = Lambdoc_core_basic
module Math = Lambdoc_core_math
module Hilite = Lambdoc_core_hilite

open Basic


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type inline =
    | Plain of string               (** Plain, unadorned text *)
    | Entity of entity              (** Unicode character entity *)
    | Linebreak                     (** Line break within same paragraph *)
    | Mathinl of Math.t             (** Inline mathematics *)
    | Code of Hilite.t              (** Inline highlighted source-code *)
    | Glyph of href * string        (** Inline image *)
    | Bold of seq                   (** Bold text *)
    | Emph of seq                   (** Emphasised text (italic) *)
    | Mono of seq                   (** Monospaced sequence *)
    | Caps of seq                   (** All-caps text *)
    | Ins of seq                    (** Text replacing wrong text *)
    | Del of seq                    (** Text to be replaced (strike-through) *)
    | Sup of seq                    (** Superscript *)
    | Sub of seq                    (** Subscript *)
    | Mbox of seq                   (** Text sequence which should not be broken across lines *)
    | Span of seq                   (** A custom span of text *)
    | Link of href * seq option     (** Reference to an external resource *)
    | See of pointer list           (** Reference to an end note *)
    | Cite of pointer list          (** Citation of a bibliography entry *)
    | Dref of pointer * seq option  (** Dumb reference to an internal element *)
    | Sref of pointer * seq option  (** Smart reference to an internal element *)
    | Mref of pointer * seq         (** Manual reference to an internal element *)

and t =
    {
    inl: inline; 
    attr: Attr.t;
    }

and seq = t list [@@deriving sexp]


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val plain:     ?attr:Attr.t -> string -> t
val entity:    ?attr:Attr.t -> entity -> t
val linebreak: ?attr:Attr.t -> unit -> t
val mathinl:   ?attr:Attr.t -> Math.t -> t
val code:      ?attr:Attr.t -> Hilite.t -> t
val glyph:     ?attr:Attr.t -> href -> string -> t
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

