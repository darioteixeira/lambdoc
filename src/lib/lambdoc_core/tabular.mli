(********************************************************************************)
(*	Tabular.mli
	Copyright (c) 2009-2013 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning tabular environments.
*)

open Prelude


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

type cellspec_t = colspec_t * int * bool * bool with sexp		(* column spec, column span, has overline, has underline *)

type cell_t = cellspec_t option * Inline.seq_t option with sexp

type row_t = cell_t nelist with sexp

type group_t = row_t nelist with sexp

type t =
	{
	tcols: colspec_t array;
	thead: group_t option;
	tfoot: group_t option;
	tbodies: group_t nelist;
	} with sexp


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

val make_cell: cellspec_t option -> Inline.seq_t option -> cell_t
val make_row: cell_t nelist -> row_t
val make_group: row_t nelist -> group_t
val make: colspec_t array -> ?thead:group_t -> ?tfoot:group_t -> group_t nelist -> t

