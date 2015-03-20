(********************************************************************************)
(*	Lambdoc_core_tabular.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type alignment_t =
	| Center
	| Left
	| Right
	| Justify

type weight_t =
	| Normal
	| Strong

type colspec_t = alignment_t * weight_t

type cellspec_t = colspec_t * int * bool * bool

type cell_t = cellspec_t option * Inline.seq_t option

type row_t = cell_t list

type group_t = row_t list

type t =
	{
	tcols: colspec_t array;
	thead: group_t option;
	tfoot: group_t option;
	tbodies: group_t list;
	}


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

let make_cell cellspec maybe_seq = (cellspec, maybe_seq)
let make_row row = row
let make_group group = group
let make tcols ?thead ?tfoot tbodies = {tcols; thead; tfoot; tbodies}

