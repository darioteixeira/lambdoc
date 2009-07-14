(********************************************************************************)
(*	Heading.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning heading elements.
*)

open Basic


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

(**	Definition of the ordering types for the various kinds of headings
*)
type part_order_t = (Order.ordinal_t, [ Order.ordinal_t Order.auto_given_t | Order.user_given_t | Order.none_given_t ]) Order.t with sexp, bin_io
type section_order_t = (Order.hierarchical_t, [Order.hierarchical_t Order.auto_given_t | Order.user_given_t | Order.none_given_t ]) Order.t with sexp, bin_io


(**	Part content.
*)
type part_content_t =
	[ `Custom of Inline.seq_t
	| `Appendix
	] with sexp, bin_io


(**	Section content.
*)
type section_content_t =
	[ `Custom of Inline.seq_t
	| `Bibliography
	| `Notes
	| `Toc
	] with sexp, bin_io


(**	Section locations.
*)
type section_location_t =
	[ `Mainbody
	| `Appendixed
	] with sexp, bin_io


(**	Heading blocks.
*)
type t =
	[ `Part of Label.t * part_order_t * part_content_t
	| `Section of Label.t * section_order_t * section_location_t * hierarchical_level_t * section_content_t
	] with sexp, bin_io


(********************************************************************************)
(**	{3 Public functions and values}						*)
(********************************************************************************)

val part: Label.t -> part_order_t -> (_, _) Inline.t list -> t

val section: Label.t -> section_order_t -> section_location_t -> hierarchical_level_t -> (_, _) Inline.t list -> t

val appendix: Label.t -> t

val bibliography: Label.t -> t

val notes: Label.t -> t

val toc: Label.t -> t

