(********************************************************************************)
(*	Entity.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Utility functions for dealing with HTML entities.
*)

(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

val is_valid: string -> bool
val get_code_point: string -> int
val iter: (string -> int -> unit) -> unit

