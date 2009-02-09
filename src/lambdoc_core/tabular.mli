(********************************************************************************)
(*	Interface file for Elem module.

	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning tabular environments.
*)

open Basic


(********************************************************************************)
(**	{2 Exceptions}								*)
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

type raw_row_t = Inline.seq_t plus_t (*with sexp*)

type 'a row_t = private raw_row_t

type raw_group_t = raw_row_t plus_t (*with sexp*)

type 'a group_t = private raw_group_t

type tabular_t =
	{
	tcols: column_t array;
	thead: raw_group_t option;
	tfoot: raw_group_t option;
	tbodies: raw_group_t plus_t;
	} (*with sexp*)

type 'a t = private tabular_t


(********************************************************************************)
(**	{3 Public functions and values}						*)
(********************************************************************************)

val column_of_specifier: char -> column_t

val alignment_to_string: alignment_t -> string

val make_row: ('a, _) Inline.t list plus_t -> 'a row_t

val make_group: 'a row_t plus_t -> 'a group_t

val make_tabular: column_t array -> ?thead:'a group_t -> ?tfoot:'a group_t -> 'a group_t plus_t -> 'a t

val get_tabular: 'a t -> tabular_t

