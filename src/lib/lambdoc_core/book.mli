(********************************************************************************)
(*	Book.mli
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions concerning books.
*)


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type isbn_t = string with sexp

type coversize_t = [ `Small | `Medium | `Large ] with sexp

type t =
	{
	title: string;
	author: string;
	publisher: string;
	pubdate: string option;
	} with sexp

