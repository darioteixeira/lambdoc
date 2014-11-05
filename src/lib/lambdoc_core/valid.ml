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
type 'a hdata_t = (Href.t, 'a) Hashtbl.t with sexp
type ('a, 'b) xdata_t = (Extkey.t, Ident.t * 'a * 'b) Hashtbl.t with sexp

type ('a, 'b, 'c, 'd) t =
	{
	content: Block.frag_t;
	bibs: Bib.t list;
	notes: Note.t list;
	toc: Heading.t list;
	labels: labels_t;
	customs: customs_t;
	links: 'a hdata_t;
	images: 'b hdata_t;
	extinls: (Extcomm.extinl_t, 'c) xdata_t;
	extblks: (Extcomm.extblk_t, 'd) xdata_t;
	} with sexp


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Constructors}							*)
(********************************************************************************)

let make ~content ~bibs ~notes ~toc ~labels ~customs ~links ~images ~extinls ~extblks =
	{content; bibs; notes; toc; labels; customs; links; images; extinls; extblks}


(********************************************************************************)
(**	{2 Serialisation facilities}						*)
(********************************************************************************)

let serialize sexp_of_a sexp_of_b sexp_of_c sexp_of_d doc =
	Sexplib.Sexp.to_string_mach (sexp_of_t sexp_of_a sexp_of_b sexp_of_c sexp_of_d doc)

let deserialize a_of_sexp b_of_sexp c_of_sexp d_of_sexp str =
	t_of_sexp a_of_sexp b_of_sexp c_of_sexp d_of_sexp (Sexplib.Sexp.of_string str)

let serialize_unitary = serialize sexp_of_unit sexp_of_unit sexp_of_unit sexp_of_unit

let deserialize_unitary = deserialize unit_of_sexp unit_of_sexp unit_of_sexp unit_of_sexp

