(********************************************************************************)
(*  Lambdoc_document_foldmap.mli
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module Valid = Lambdoc_document_valid

open Lambdoc_prelude
open Valid


(********************************************************************************)
(** {1 Public signatures}                                                       *)
(********************************************************************************)

module type S =
sig
    module IO: Monad.S


    (****************************************************************************)
    (** {2 Type definitions}                                                    *)
    (****************************************************************************)

    (** Type of the foldmapper itself.
    *)
    type 'a t =
        {
        (** Top level manipulation function *)

        valid:       'a t -> 'a -> Valid.t -> ('a * Valid.t) IO.t;

        (** Atribute manipulation function *)

        attr:        'a t -> 'a -> Attr.t -> ('a * Attr.t) IO.t;

        (** Bulk manipulation functions *)

        inline:      'a t -> 'a -> Inline.t -> ('a * Inline.t) IO.t;
        block:       'a t -> 'a -> Block.t -> ('a * Block.t) IO.t;
        seq:         'a t -> 'a -> Inline.seq -> ('a * Inline.seq) IO.t;
        frag:        'a t -> 'a -> Block.frag -> ('a * Block.frag) IO.t;

        (** Functions for manipulating inline context elements *)

        plain:       'a t -> 'a -> Attr.t -> string -> ('a * Inline.t) IO.t;
        entity:      'a t -> 'a -> Attr.t -> entity -> ('a * Inline.t) IO.t;
        linebreak:   'a t -> 'a -> Attr.t -> ('a * Inline.t) IO.t;
        math_inl:    'a t -> 'a -> Attr.t -> Math.t -> ('a * Inline.t) IO.t;
        code:        'a t -> 'a -> Attr.t -> Hilite.t -> ('a * Inline.t) IO.t;
        glyph:       'a t -> 'a -> Attr.t -> href -> string -> string option -> ('a * Inline.t) IO.t;
        bold:        'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        emph:        'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        mono:        'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        caps:        'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        ins:         'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        del:         'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        sup:         'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        sub:         'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        mbox:        'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        span:        'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t;
        link:        'a t -> 'a -> Attr.t -> href -> Inline.seq option -> ('a * Inline.t) IO.t;
        see:         'a t -> 'a -> Attr.t -> pointer list -> ('a * Inline.t) IO.t;
        cite:        'a t -> 'a -> Attr.t -> pointer list -> ('a * Inline.t) IO.t;
        dref:        'a t -> 'a -> Attr.t -> pointer -> Inline.seq option -> ('a * Inline.t) IO.t;
        sref:        'a t -> 'a -> Attr.t -> pointer -> Inline.seq option -> ('a * Inline.t) IO.t;
        mref:        'a t -> 'a -> Attr.t -> pointer -> Inline.seq -> ('a * Inline.t) IO.t;

        (** Functions for manipulating block context elements *)

        paragraph:   'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Block.t) IO.t;
        itemize:     'a t -> 'a -> Attr.t -> Block.frag list -> ('a * Block.t) IO.t;
        enumerate:   'a t -> 'a -> Attr.t -> Block.frag list -> ('a * Block.t) IO.t;
        description: 'a t -> 'a -> Attr.t -> (Inline.seq * Block.frag) list -> ('a * Block.t) IO.t;
        qanda:       'a t -> 'a -> Attr.t -> (Qanda.t * Block.frag) list -> ('a * Block.t) IO.t;
        verse:       'a t -> 'a -> Attr.t -> Block.frag -> ('a * Block.t) IO.t;
        quote:       'a t -> 'a -> Attr.t -> Block.frag -> ('a * Block.t) IO.t;
        math_blk:    'a t -> 'a -> Attr.t -> Math.t -> ('a * Block.t) IO.t;
        source:      'a t -> 'a -> Attr.t -> Hilite.t -> ('a * Block.t) IO.t;
        tabular:     'a t -> 'a -> Attr.t -> Tabular.t -> ('a * Block.t) IO.t;
        subpage:     'a t -> 'a -> Attr.t -> Block.frag -> ('a * Block.t) IO.t;
        verbatim:    'a t -> 'a -> Attr.t -> string -> ('a * Block.t) IO.t;
        picture:     'a t -> 'a -> Attr.t -> href -> string -> string option -> int option -> ('a * Block.t) IO.t;
        pullquote:   'a t -> 'a -> Attr.t -> Inline.seq option -> Block.frag -> ('a * Block.t) IO.t;
        boxout:      'a t -> 'a -> Attr.t -> Custom.boxout -> Inline.seq option -> Block.frag -> ('a * Block.t) IO.t;
        theorem:     'a t -> 'a -> Attr.t -> Custom.theorem -> Inline.seq option -> Block.frag -> ('a * Block.t) IO.t;
        equation:    'a t -> 'a -> Attr.t -> Wrapper.t -> Block.t -> ('a * Block.t) IO.t;
        printout:    'a t -> 'a -> Attr.t -> Wrapper.t -> Block.t -> ('a * Block.t) IO.t;
        table:       'a t -> 'a -> Attr.t -> Wrapper.t -> Block.t -> ('a * Block.t) IO.t;
        figure:      'a t -> 'a -> Attr.t -> Wrapper.t -> Block.t -> ('a * Block.t) IO.t;
        heading:     'a t -> 'a -> Attr.t -> Heading.t -> ('a * Block.t) IO.t;
        autogen:     'a t -> 'a -> Attr.t -> Autogen.t -> ('a * Block.t) IO.t;
        title:       'a t -> 'a -> Attr.t -> Level.title -> Inline.seq -> ('a * Block.t) IO.t;
        abstract:    'a t -> 'a -> Attr.t -> Block.frag -> ('a * Block.t) IO.t;
        rule:        'a t -> 'a -> Attr.t -> ('a * Block.t) IO.t;
        }


    (****************************************************************************)
    (** {2 Auxiliary functions}                                                 *)
    (****************************************************************************)

    val aux_list: ('a t -> 'a -> 'b -> ('a * 'c) IO.t) -> 'a t -> 'a -> 'b list -> ('a * 'c list) IO.t
    val aux_maybe: ('a t -> 'a -> 'b -> ('a * 'c) IO.t) -> 'a t -> 'a -> 'b option -> ('a * 'c option) IO.t
    val aux_seq: (?attr:Attr.t -> Inline.seq -> Inline.t) -> 'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) IO.t
    val aux_frag: (?attr:Attr.t -> Block.frag -> Block.t) -> 'a t -> 'a -> Attr.t -> Block.frag -> ('a * Block.t) IO.t
    val aux_frags: (?attr:Attr.t -> Block.frag list -> Block.t) -> 'a t -> 'a -> Attr.t -> Block.frag list -> ('a * Block.t) IO.t
    val aux_wrapper: (?attr:Attr.t -> Wrapper.t -> Block.t -> Block.t) -> 'a t -> 'a -> Attr.t -> Wrapper.t -> Block.t -> ('a * Block.t) IO.t


    (****************************************************************************)
    (** {2 Pre-built foldmappers}                                               *)
    (****************************************************************************)

    (** The identity foldmapper traverses the entire document tree, outputting
        a new document identical in every aspect to the original one.  It provides
        a useful base for writing custom foldmappers.
    *)
    val identity: 'a t

    (** The amnesiac foldmapper is based upon on the {!identity} foldmapper.
        The sole difference is that it drops all the parsing information from
        every attribute in the document tree.  The resulting document is thus
        rendered amnesiac about its parsing history.
    *)
    val amnesiac: 'a t
end


(********************************************************************************)
(** {1 Public modules and functors}                                             *)
(********************************************************************************)

module Make: functor (M: Monad.S) -> S with module IO = M

module Identity: S with module IO = Monad.Identity

