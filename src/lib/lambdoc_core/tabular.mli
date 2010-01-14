(********************************************************************************)
(*	Tabular.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning tabular environments.
*)

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

type raw_cell_t = cellspec_t option * Inline.seq_t option with sexp

type 'a cell_t = private raw_cell_t

type raw_row_t = raw_cell_t nelist with sexp

type 'a row_t = private raw_row_t

type raw_group_t = raw_row_t nelist with sexp

type 'a group_t = private raw_group_t

type tabular_t =
	{
	tcols: colspec_t array;
	thead: raw_group_t option;
	tfoot: raw_group_t option;
	tbodies: raw_group_t nelist;
	} with sexp

type 'a t = private tabular_t


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

val make_cell: cellspec_t option -> ('a, _) Inline.t nelist option -> 'a cell_t
val make_row: 'a cell_t nelist -> 'a row_t
val make_group: 'a row_t nelist -> 'a group_t
val make_tabular: colspec_t array -> ?thead:'a group_t -> ?tfoot:'a group_t -> 'a group_t nelist -> 'a t
val get_tabular: 'a t -> tabular_t

