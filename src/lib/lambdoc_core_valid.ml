(********************************************************************************)
(*  Lambdoc_core_valid.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module Basic = Lambdoc_core_basic
module Bib = Lambdoc_core_bib
module Block = Lambdoc_core_block
module Custom = Lambdoc_core_custom
module Heading = Lambdoc_core_heading
module Inline = Lambdoc_core_inline
module Label = Lambdoc_core_label
module Note = Lambdoc_core_note
module Target = Lambdoc_core_target

open Sexplib.Std
open Basic


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type labels = (Label.t, Target.t) Hashtbl.t with sexp
type customs = (Custom.key, Inline.seq) Hashtbl.t with sexp

type t =
    {
    content: Block.frag;
    bibs: Bib.t list;
    notes: Note.t list;
    toc: Heading.t list;
    labels: labels;
    customs: customs;
    } with sexp


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

(********************************************************************************)
(** {2 Constructors}                                                            *)
(********************************************************************************)

let make ?(bibs = []) ?(notes = []) ?(toc = []) ?(labels = Hashtbl.create 0) ?(customs = Hashtbl.create 0) content =
    {content; bibs; notes; toc; labels; customs}


(********************************************************************************)
(** {2 Serialisation facilities}                                                *)
(********************************************************************************)

let serialize doc =
    Sexplib.Sexp.to_string_mach (sexp_of_t doc)

let deserialize str =
    t_of_sexp (Sexplib.Sexp.of_string str)

