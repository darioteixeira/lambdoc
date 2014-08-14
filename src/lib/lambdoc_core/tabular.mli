(********************************************************************************)
(*	Tabular.mli
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning tabular environments.
*)


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

type row_t = cell_t list with sexp

type group_t = row_t list with sexp

type t =
	{
	tcols: colspec_t array;
	thead: group_t option;
	tfoot: group_t option;
	tbodies: group_t list;
	} with sexp


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

val make_cell: cellspec_t option -> Inline.seq_t option -> cell_t
val make_row: cell_t list -> row_t
val make_group: row_t list -> group_t
val make: colspec_t array -> ?thead:group_t -> ?tfoot:group_t -> group_t list -> t

