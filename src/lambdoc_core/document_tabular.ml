(********************************************************************************)
(*	Implementation file for Document_tabular.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of tabular environments.
*)

TYPE_CONV_PATH "Document"

open Document_basic
open Document_node


(********************************************************************************)
(*	{2 Tabular module}							*)
(********************************************************************************)

module Tabular:
sig
	exception Invalid_column_specifier of char

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

	type row_t = Node.super_seq_t plus_t (*with sexp*)

	type group_t = row_t plus_t (*with sexp*)

	type t =
		{
		tcols: column_t array;
		thead: group_t option;
		tfoot: group_t option;
		tbodies: group_t plus_t;
		} (*with sexp*)

	val column_of_specifier: char -> column_t
	val alignment_to_string: alignment_t -> string
	val make_row: ([< Node.super_node_t ], 'b) Node.t list plus_t -> row_t
	val make: column_t array -> ?thead:group_t -> ?tfoot:group_t -> group_t plus_t -> t
end =
struct
	exception Invalid_column_specifier of char

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

	type row_t = Node.super_seq_t plus_t (*with sexp*)

	type group_t = row_t plus_t (*with sexp*)

	type t =
		{
		tcols: column_t array;
		thead: group_t option;
		tfoot: group_t option;
		tbodies: group_t plus_t;
		} (*with sexp*)

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

	let make_row (hd, tl) = ((hd :> Node.super_seq_t), (tl :> Node.super_seq_t list))

	let make tcols ?thead ?tfoot tbodies =
		{
		tcols = tcols;
		thead = thead;
		tfoot = tfoot;
		tbodies = tbodies;
		}
end

