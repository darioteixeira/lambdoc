(********************************************************************************)
(*	Prelude.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of common types and functions which though not defined
	in Pervasives, probably should.
*)

open Sexplib.Std


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

(**	The type of non-empty lists.
*)
type 'a nelist = 'a * 'a list with sexp


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

(**	Similar to [List.map], but for values of type {!nelist}.
*)
let nemap func (hd, tl) =
	(func hd, List.map func tl)


(**	Possibly apply a function.
*)
let maybe f = function
	| Some x -> Some (f x)
	| None	 -> None


(**	Identity function.
*)
external identity: 'a -> 'a = "%identity"


(**	Pipe operator.
*)
let (|>) x f = f x

