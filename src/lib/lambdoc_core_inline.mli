(********************************************************************************)
(*  Lambdoc_core_inline.mli
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Definitions concerning inline elements.
*)

module Attr = Lambdoc_core_attr
module Basic = Lambdoc_core_basic
module Math = Lambdoc_core_math

open Basic


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type inline_t =
    | Plain of string                   (** Plain, unadorned text *)
    | Entity of entity_t                (** Unicode character entity *)
    | Linebreak                         (** Line break within same paragraph *)
    | Mathinl of Math.t                 (** Inline mathematics *)
    | Glyph of href_t * string          (** Inline image *)
    | Bold of seq_t                     (** Bold text *)
    | Emph of seq_t                     (** Emphasised text (italic) *)
    | Code of seq_t                     (** Inline source-code sequence (monospaced) *)
    | Caps of seq_t                     (** All-caps text *)
    | Ins of seq_t                      (** Text replacing wrong text *)
    | Del of seq_t                      (** Text to be replaced (strike-through) *)
    | Sup of seq_t                      (** Superscript *)
    | Sub of seq_t                      (** Subscript *)
    | Mbox of seq_t                     (** Text sequence which should not be broken across lines *)
    | Span of seq_t                     (** A custom span of text *)
    | Link of href_t * seq_t option     (** Reference to an external resource *)
    | See of pointer_t list             (** Reference to an end note *)
    | Cite of pointer_t list            (** Citation of a bibliography entry *)
    | Dref of pointer_t * seq_t option  (** Dumb reference to an internal element *)
    | Sref of pointer_t * seq_t option  (** Smart reference to an internal element *)
    | Mref of pointer_t * seq_t         (** Manual reference to an internal element *)

and t =
    {
    inl: inline_t; 
    attr: Attr.t;
    }

and seq_t = t list with sexp


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val plain:     ?attr:Attr.t -> string -> t
val entity:    ?attr:Attr.t -> entity_t -> t
val linebreak: ?attr:Attr.t -> unit -> t
val mathinl:   ?attr:Attr.t -> Math.t -> t
val glyph:     ?attr:Attr.t -> href_t -> string -> t
val bold:      ?attr:Attr.t -> seq_t -> t
val emph:      ?attr:Attr.t -> seq_t -> t
val code:      ?attr:Attr.t -> seq_t -> t
val caps:      ?attr:Attr.t -> seq_t -> t
val ins:       ?attr:Attr.t -> seq_t -> t
val del:       ?attr:Attr.t -> seq_t -> t
val sup:       ?attr:Attr.t -> seq_t -> t
val sub:       ?attr:Attr.t -> seq_t -> t
val mbox:      ?attr:Attr.t -> seq_t -> t
val span:      ?attr:Attr.t -> seq_t -> t
val link:      ?attr:Attr.t -> href_t -> seq_t option -> t
val see:       ?attr:Attr.t -> pointer_t list -> t
val cite:      ?attr:Attr.t -> pointer_t list -> t
val dref:      ?attr:Attr.t -> pointer_t -> seq_t option -> t
val sref:      ?attr:Attr.t -> pointer_t -> seq_t option -> t
val mref:      ?attr:Attr.t -> pointer_t -> seq_t -> t

