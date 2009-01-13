(********************************************************************************)
(*	Implementation file for Labelmap module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(*TYPE_CONV_PATH "Document"*)


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type key_t = Label.t
type value_t = Target.t
type t = (key_t, value_t) Hashtbl.t


(********************************************************************************)
(**	{2 Functions and values}						*)
(********************************************************************************)

let create () = Hashtbl.create 10

let add = Hashtbl.add

let mem = Hashtbl.mem

let find = Hashtbl.find

