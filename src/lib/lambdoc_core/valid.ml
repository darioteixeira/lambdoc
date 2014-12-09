(********************************************************************************)
(*	Valid.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Sexplib.Std
open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type labels_t = (Label.t, Target.t) Hashtbl.t with sexp
type customs_t = (Custom.key_t, Inline.seq_t) Hashtbl.t with sexp
type hdata_t = (Href.t, string option) Hashtbl.t with sexp

type t =
	{
	content: Block.frag_t;
	bibs: Bib.t list;
	notes: Note.t list;
	toc: Heading.t list;
	labels: labels_t;
	customs: customs_t;
	links: hdata_t;
	images: hdata_t;
	} with sexp


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Constructors}							*)
(********************************************************************************)

let make ~content ~bibs ~notes ~toc ~labels ~customs ~links ~images =
	{content; bibs; notes; toc; labels; customs; links; images;}


(********************************************************************************)
(**	{2 Serialisation facilities}						*)
(********************************************************************************)

let serialize doc =
	Sexplib.Sexp.to_string_mach (sexp_of_t doc)

let deserialize str =
	t_of_sexp (Sexplib.Sexp.of_string str)

