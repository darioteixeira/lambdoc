(********************************************************************************)
(*	Lambdoc_core_heading.ml
	Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

module Basic = Lambdoc_core_basic
module Inline = Lambdoc_core_inline
module Label = Lambdoc_core_label
module Order = Lambdoc_core_order

open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type part_order_t = (Order.ordinal_t, [ Order.ordinal_t Order.auto_given_t | Order.ordinal_t Order.user_given_t | Order.none_given_t ]) Order.t with sexp

type section_order_t = (Order.hierarchical_t, [Order.hierarchical_t Order.auto_given_t | Order.hierarchical_t Order.user_given_t | Order.none_given_t ]) Order.t with sexp

type part_content_t =
	| Custom_part of Inline.seq_t
	| Appendix
	with sexp

type section_content_t =
	| Custom_section of Inline.seq_t
	| Bibliography
	| Notes
	| Toc
	with sexp

type section_location_t =
	| Mainbody
	| Appendixed
	with sexp

type t =
	| Part of Label.t * part_order_t * part_content_t
	| Section of Label.t * section_order_t * section_location_t * Level.section_t * section_content_t
	with sexp


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

let part label order seq = Part (label, order, Custom_part seq)
let appendix label = Part (label, `None_given, Appendix)
let section label order location level seq = Section (label, order, location, level, Custom_section seq)
let bibliography label = Section (label, `None_given, Mainbody, Level.section 1, Bibliography)
let notes label = Section (label, `None_given, Mainbody, Level.section 1, Notes)
let toc label = Section (label, `None_given, Mainbody, Level.section 1, Toc)

