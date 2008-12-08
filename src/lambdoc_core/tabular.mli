(********************************************************************************)
(*	Interface file for Tabular module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Tabular environments.
*)

open Basic


(********************************************************************************)
(**	{Exceptions}								*)
(********************************************************************************)

exception Invalid_column_specifier of char


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type alignment_t =
	| Center
	| Left
	| Right
	| Justify
	(*with sexp*)

type weight_t =
	| Normal
	| Strong
	(*with sexp*)

type column_t = alignment_t * weight_t (*with sexp*)

type row_t = Node.M.super_seq_t plus_t (*with sexp*)

type group_t = row_t plus_t (*with sexp*)

type t =
	{
	tcols: column_t array;
	thead: group_t option;
	tfoot: group_t option;
	tbodies: group_t plus_t;
	} (*with sexp*)


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

val column_of_specifier: char -> column_t

val alignment_to_string: alignment_t -> string

val make_row: ([< Node.M.super_node_t ], 'b) Node.M.t list plus_t -> row_t

val make: column_t array -> ?thead:group_t -> ?tfoot:group_t -> group_t plus_t -> t

