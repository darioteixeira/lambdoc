(********************************************************************************)
(*	Tabular.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

TYPE_CONV_PATH "Tabular"

open Prelude
open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type alignment_t =
	| Center
	| Left
	| Right
	| Justify
	with sexp

type weight_t =
	| Normal
	| Strong
	with sexp

type colspec_t = alignment_t * weight_t with sexp

type cellspec_t = colspec_t * int * bool with sexp

type raw_cell_t = cellspec_t option * Inline.seq_t with sexp

type 'a cell_t = raw_cell_t

type raw_row_t = raw_cell_t nelist with sexp

type 'a row_t = raw_row_t

type raw_group_t = raw_row_t nelist with sexp

type 'a group_t = raw_group_t

type tabular_t =
	{
	tcols: colspec_t array;
	thead: raw_group_t option;
	tfoot: raw_group_t option;
	tbodies: raw_group_t nelist;
	} with sexp

type 'a t = tabular_t


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

let make_cell cellspec seq = (cellspec, Inline.get_seq seq)

let make_row (hd, tl) = (hd, tl)

let make_group (hd, tl) = (hd, tl)

let make_tabular tcols ?thead ?tfoot tbodies =
	{
	tcols = tcols;
	thead = thead;
	tfoot = tfoot;
	tbodies = tbodies;
	}

let get_tabular tab = tab

