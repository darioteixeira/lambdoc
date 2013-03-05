(********************************************************************************)
(*	Heading.mli
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning heading elements.
*)

open Prelude
open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

(**	Definition of the ordering types for the various kinds of headings
*)

type part_order_t = (Order.ordinal_t, [ Order.ordinal_t Order.auto_given_t | Order.ordinal_t Order.user_given_t | Order.none_given_t ]) Order.t with sexp
type section_order_t = (Order.hierarchical_t, [Order.hierarchical_t Order.auto_given_t | Order.hierarchical_t Order.user_given_t | Order.none_given_t ]) Order.t with sexp


(**	Part content.
*)
type part_content_t =
	| Custom_part of Inline.seq_t
	| Appendix
	with sexp


(**	Section content.
*)
type section_content_t =
	| Custom_section of Inline.seq_t
	| Bibliography
	| Notes
	| Toc
	with sexp


(**	Section locations.
*)
type section_location_t =
	| Mainbody
	| Appendixed
	with sexp


(**	Heading blocks.
*)
type t =
	| Part of Label.t * part_order_t * part_content_t
	| Section of Label.t * section_order_t * section_location_t * Level.hierarchical_t * section_content_t
	with sexp


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

val part: Label.t -> part_order_t -> Inline.seq_t -> t
val appendix: Label.t -> t
val section: Label.t -> section_order_t -> section_location_t -> Level.hierarchical_t -> Inline.seq_t -> t
val bibliography: Label.t -> t
val notes: Label.t -> t
val toc: Label.t -> t

