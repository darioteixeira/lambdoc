(********************************************************************************)
(*	Book.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning books.
*)

open Sexplib.Std


(********************************************************************************)
(**	{1 Exceptions}								*)
(********************************************************************************)

exception Malformed_ISBN of string
exception Unknown_ISBN of string


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type isbn_t = string with sexp

type rating_t = int with sexp

type coversize_t =
	| Small
	| Medium
	| Large
	with sexp

type t =
	{
	title: string;
	author: string;
	publisher: string;
	pubdate: string option;
	} with sexp

type maker_t = isbn_t -> t Lwt.t

