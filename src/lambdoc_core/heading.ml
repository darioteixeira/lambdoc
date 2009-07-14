(********************************************************************************)
(*	Heading.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

TYPE_CONV_PATH "Heading"

open Basic


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type part_order_t = (Order.ordinal_t, [ Order.ordinal_t Order.auto_given_t | Order.user_given_t | Order.none_given_t ]) Order.t with sexp, bin_io
type section_order_t = (Order.hierarchical_t, [Order.hierarchical_t Order.auto_given_t | Order.user_given_t | Order.none_given_t ]) Order.t with sexp, bin_io

type part_content_t =
	[ `Custom of Inline.seq_t
	| `Appendix
	] with sexp, bin_io

type section_content_t =
	[ `Custom of Inline.seq_t
	| `Bibliography
	| `Notes
	| `Toc
	] with sexp, bin_io

type section_location_t =
	[ `Mainbody
	| `Appendixed
	] with sexp, bin_io

type t =
	[ `Part of Label.t * part_order_t * part_content_t
	| `Section of Label.t * section_order_t * section_location_t * hierarchical_level_t * section_content_t
	] with sexp, bin_io


(********************************************************************************)
(**	{3 Functions and values}						*)
(********************************************************************************)

let part label order seq = `Part (label, order, `Custom (Inline.get_seq seq))
let section label order location level seq = `Section (label, order, location, level, `Custom (Inline.get_seq seq))
let appendix label = `Part (label, Order.none (), `Appendix)
let bibliography label = `Section (label, Order.none (), `Mainbody, `Level1, `Bibliography)
let notes label = `Section (label, Order.none (), `Mainbody, `Level1, `Notes)
let toc label = `Section (label, Order.none (), `Mainbody, `Level1, `Toc)

