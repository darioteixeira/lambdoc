(********************************************************************************)
(*	Implementation file for Tabular module.

	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

TYPE_CONV_PATH "Tabular"

open Basic


(********************************************************************************)
(**	{3 Exceptions}								*)
(********************************************************************************)

exception Invalid_column_specifier of char


(********************************************************************************)
(**	{3 Type definitions}							*)
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

type column_t = alignment_t * weight_t with sexp

type raw_row_t = Inline.seq_t plus_t with sexp

type 'a row_t = raw_row_t

type raw_group_t = raw_row_t plus_t with sexp

type 'a group_t = raw_group_t

type tabular_t =
	{
	tcols: column_t array;
	thead: raw_group_t option;
	tfoot: raw_group_t option;
	tbodies: raw_group_t plus_t;
	} with sexp

type 'a t = tabular_t


(********************************************************************************)
(**	{3 Functions and values}						*)
(********************************************************************************)

let column_of_specifier = function
	| 'c' -> (Center, Normal)
	| 'C' -> (Center, Strong)
	| 'l' -> (Left, Normal)
	| 'L' -> (Left, Strong)
	| 'r' -> (Right, Normal)
	| 'R' -> (Right, Strong)
	| 'j' -> (Justify, Normal)
	| 'J' -> (Justify, Strong)
	| x   -> raise (Invalid_column_specifier x)

let alignment_to_string = function
	| Center	-> "center"
	| Left		-> "left"
	| Right		-> "right"
	| Justify	-> "justify"

let make_row seqs = Inline.get_seqs seqs

let make_group (hd, tl) = (hd, tl)

let make_tabular tcols ?thead ?tfoot tbodies =
	{
	tcols = tcols;
	thead = thead;
	tfoot = tfoot;
	tbodies = tbodies;
	}

let get_tabular tab = tab
