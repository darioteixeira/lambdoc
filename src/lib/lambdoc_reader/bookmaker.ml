(********************************************************************************)
(*	Bookmaker.ml
	Copyright (c) 2009-2013 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Bookmaker
*)

open Lambdoc_core


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type failure_t =
	| Unavailable
	| Uncapable of string
	| Malformed_ISBN of string
	| Unknown_ISBN of string


type result_t =
	| Success of Book.t
	| Failure of failure_t


type t = Book.isbn_t list -> (Book.isbn_t * result_t) list

