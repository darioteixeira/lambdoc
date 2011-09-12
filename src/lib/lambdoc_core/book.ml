(********************************************************************************)
(*	Book.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning books.
*)

(********************************************************************************)
(**	{1 Exceptions}								*)
(********************************************************************************)

exception Malformed_isbn
exception Unknown_isbn


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type isbn_t =
	| Only10 of string
	| Only13 of string
	| Both of string * string
	with sexp

type info_t =
	{
	title: string;
	author: string;
	publisher: string;
	year: int;
	isbn: isbn_t;
	rating: int option;
	} with sexp

type cover_t =
	| Small
	| Medium
	| Large
	with sexp

type maker_t = string -> int option -> info_t

