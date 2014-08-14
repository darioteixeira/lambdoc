(********************************************************************************)
(*	Prelude.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of common functions which though not defined in Pervasives,
	probably should.
*)


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

external identity: 'a -> 'a = "%identity"


let maybe f = function
	| Some x -> Some (f x)
	| None	 -> None

