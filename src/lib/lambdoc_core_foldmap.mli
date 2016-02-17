(********************************************************************************)
(*  Lambdoc_core_foldmap.mli
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module Attr = Lambdoc_core_attr
module Basic = Lambdoc_core_basic
module Block = Lambdoc_core_block
module Custom = Lambdoc_core_custom
module Heading = Lambdoc_core_heading
module Inline = Lambdoc_core_inline
module Level = Lambdoc_core_level
module Math = Lambdoc_core_math
module Monadic = Lambdoc_core_monadic
module Qanda = Lambdoc_core_qanda
module Hilite = Lambdoc_core_hilite
module Tabular = Lambdoc_core_tabular
module Valid = Lambdoc_core_valid
module Wrapper = Lambdoc_core_wrapper

open Basic


(********************************************************************************)
(** {1 Public signatures}                                                       *)
(********************************************************************************)

module type S =
sig
    module Monad: Monadic.S


    (****************************************************************************)
    (** {2 Type definitions}                                                    *)
    (****************************************************************************)

    (** Type of the foldmapper itself.
    *)
    type 'a t =
        {
        (** Top level manipulation function *)

        valid:       'a t -> 'a -> Valid.t -> ('a * Valid.t) Monad.t;

        (** Atribute manipulation function *)

        attr:        'a t -> 'a -> Attr.t -> ('a * Attr.t) Monad.t;

        (** Bulk manipulation functions *)

        inline:      'a t -> 'a -> Inline.t -> ('a * Inline.t) Monad.t;
        block:       'a t -> 'a -> Block.t -> ('a * Block.t) Monad.t;
        seq:         'a t -> 'a -> Inline.seq -> ('a * Inline.seq) Monad.t;
        frag:        'a t -> 'a -> Block.frag -> ('a * Block.frag) Monad.t;

        (** Functions for manipulating inline context elements *)

        plain:       'a t -> 'a -> Attr.t -> string -> ('a * Inline.t) Monad.t;
        entity:      'a t -> 'a -> Attr.t -> entity -> ('a * Inline.t) Monad.t;
        linebreak:   'a t -> 'a -> Attr.t -> ('a * Inline.t) Monad.t;
        math_inl:    'a t -> 'a -> Attr.t -> Math.t -> ('a * Inline.t) Monad.t;
        code:        'a t -> 'a -> Attr.t -> Hilite.t -> ('a * Inline.t) Monad.t;
        glyph:       'a t -> 'a -> Attr.t -> href -> string -> ('a * Inline.t) Monad.t;
        bold:        'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) Monad.t;
        emph:        'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) Monad.t;
        mono:        'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) Monad.t;
        caps:        'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) Monad.t;
        ins:         'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) Monad.t;
        del:         'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) Monad.t;
        sup:         'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) Monad.t;
        sub:         'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) Monad.t;
        mbox:        'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) Monad.t;
        span:        'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) Monad.t;
        link:        'a t -> 'a -> Attr.t -> href -> Inline.seq option -> ('a * Inline.t) Monad.t;
        see:         'a t -> 'a -> Attr.t -> pointer list -> ('a * Inline.t) Monad.t;
        cite:        'a t -> 'a -> Attr.t -> pointer list -> ('a * Inline.t) Monad.t;
        dref:        'a t -> 'a -> Attr.t -> pointer -> Inline.seq option -> ('a * Inline.t) Monad.t;
        sref:        'a t -> 'a -> Attr.t -> pointer -> Inline.seq option -> ('a * Inline.t) Monad.t;
        mref:        'a t -> 'a -> Attr.t -> pointer -> Inline.seq -> ('a * Inline.t) Monad.t;

        (** Functions for manipulating block context elements *)

        paragraph:   'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Block.t) Monad.t;
        itemize:     'a t -> 'a -> Attr.t -> Block.frag list -> ('a * Block.t) Monad.t;
        enumerate:   'a t -> 'a -> Attr.t -> Block.frag list -> ('a * Block.t) Monad.t;
        description: 'a t -> 'a -> Attr.t -> (Inline.seq * Block.frag) list -> ('a * Block.t) Monad.t;
        qanda:       'a t -> 'a -> Attr.t -> (Qanda.t * Block.frag) list -> ('a * Block.t) Monad.t;
        verse:       'a t -> 'a -> Attr.t -> Block.frag -> ('a * Block.t) Monad.t;
        quote:       'a t -> 'a -> Attr.t -> Block.frag -> ('a * Block.t) Monad.t;
        math_blk:    'a t -> 'a -> Attr.t -> Math.t -> ('a * Block.t) Monad.t;
        source:      'a t -> 'a -> Attr.t -> Hilite.t -> ('a * Block.t) Monad.t;
        tabular:     'a t -> 'a -> Attr.t -> Tabular.t -> ('a * Block.t) Monad.t;
        subpage:     'a t -> 'a -> Attr.t -> Block.frag -> ('a * Block.t) Monad.t;
        verbatim:    'a t -> 'a -> Attr.t -> string -> ('a * Block.t) Monad.t;
        picture:     'a t -> 'a -> Attr.t -> href -> string -> int option -> ('a * Block.t) Monad.t;
        pullquote:   'a t -> 'a -> Attr.t -> Inline.seq option -> Block.frag -> ('a * Block.t) Monad.t;
        boxout:      'a t -> 'a -> Attr.t -> Custom.Boxout.t -> Inline.seq option -> Block.frag -> ('a * Block.t) Monad.t;
        theorem:     'a t -> 'a -> Attr.t -> Custom.Theorem.t -> Inline.seq option -> Block.frag -> ('a * Block.t) Monad.t;
        equation:    'a t -> 'a -> Attr.t -> Wrapper.t -> Block.t -> ('a * Block.t) Monad.t;
        printout:    'a t -> 'a -> Attr.t -> Wrapper.t -> Block.t -> ('a * Block.t) Monad.t;
        table:       'a t -> 'a -> Attr.t -> Wrapper.t -> Block.t -> ('a * Block.t) Monad.t;
        figure:      'a t -> 'a -> Attr.t -> Wrapper.t -> Block.t -> ('a * Block.t) Monad.t;
        heading:     'a t -> 'a -> Attr.t -> Heading.t -> ('a * Block.t) Monad.t;
        title:       'a t -> 'a -> Attr.t -> Level.title -> Inline.seq -> ('a * Block.t) Monad.t;
        abstract:    'a t -> 'a -> Attr.t -> Block.frag -> ('a * Block.t) Monad.t;
        rule:        'a t -> 'a -> Attr.t -> ('a * Block.t) Monad.t;
        }


    (****************************************************************************)
    (** {2 Auxiliary functions}                                                 *)
    (****************************************************************************)

    val aux_list: ('a t -> 'a -> 'b -> ('a * 'c) Monad.t) -> 'a t -> 'a -> 'b list -> ('a * 'c list) Monad.t
    val aux_maybe: ('a t -> 'a -> 'b -> ('a * 'c) Monad.t) -> 'a t -> 'a -> 'b option -> ('a * 'c option) Monad.t
    val aux_seq: (?attr:Attr.t -> Inline.seq -> Inline.t) -> 'a t -> 'a -> Attr.t -> Inline.seq -> ('a * Inline.t) Monad.t
    val aux_frag: (?attr:Attr.t -> Block.frag -> Block.t) -> 'a t -> 'a -> Attr.t -> Block.frag -> ('a * Block.t) Monad.t
    val aux_frags: (?attr:Attr.t -> Block.frag list -> Block.t) -> 'a t -> 'a -> Attr.t -> Block.frag list -> ('a * Block.t) Monad.t
    val aux_wrapper: (?attr:Attr.t -> Wrapper.t -> Block.t -> Block.t) -> 'a t -> 'a -> Attr.t -> Wrapper.t -> Block.t -> ('a * Block.t) Monad.t


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

module Make: functor (M: Monadic.S) -> S with module Monad = M

module Identity: S with module Monad = Monadic.Identity

