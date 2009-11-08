(********************************************************************************)
(*	Tabular.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning tabular environments.
*)

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

type cellspec_t = colspec_t * int with sexp

type raw_cell_t = cellspec_t option * Inline.seq_t with sexp

type 'a cell_t = private raw_cell_t

type raw_row_t = raw_cell_t plus_t with sexp

type 'a row_t = private raw_row_t

type raw_group_t = raw_row_t plus_t with sexp

type 'a group_t = private raw_group_t

type tabular_t =
	{
	tcols: colspec_t array;
	thead: raw_group_t option;
	tfoot: raw_group_t option;
	tbodies: raw_group_t plus_t;
	} with sexp

type 'a t = private tabular_t


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

val colspec_of_string: string -> colspec_t

val string_of_alignment: alignment_t -> string

val make_cell: cellspec_t option -> ('a, _) Inline.t list -> 'a cell_t

val make_row: 'a cell_t plus_t -> 'a row_t

val make_group: 'a row_t plus_t -> 'a group_t

val make_tabular: colspec_t array -> ?thead:'a group_t -> ?tfoot:'a group_t -> 'a group_t plus_t -> 'a t

val get_tabular: 'a t -> tabular_t

