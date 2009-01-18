(********************************************************************************)
(*	Implementation file for Tabular module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

TYPE_CONV_PATH "Document"

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
	with sexp

type weight_t =
	| Normal
	| Strong
	with sexp

type column_t = alignment_t * weight_t with sexp

type row_t = Node.M.super_seq_t plus_t with sexp

type group_t = row_t plus_t with sexp

type t =
	{
	tcols: column_t array;
	thead: group_t option;
	tfoot: group_t option;
	tbodies: group_t plus_t;
	} with sexp


(********************************************************************************)
(**	{2 Public functions and values}						*)
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

let make_row (hd, tl) = ((hd :> Node.M.super_seq_t), (tl :> Node.M.super_seq_t list))

let make tcols ?thead ?tfoot tbodies =
	{
	tcols = tcols;
	thead = thead;
	tfoot = tfoot;
	tbodies = tbodies;
	}

