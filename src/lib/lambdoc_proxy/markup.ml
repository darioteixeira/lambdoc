(********************************************************************************)
(*	Markup.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)


(********************************************************************************)
(**	{1 Module signatures}							*)
(********************************************************************************)

module type S =
sig
	type t =
		| Lambtex
		| Lambhtml
		| Lamblite

	val of_string: string -> t
	val to_string: t -> string
end


(********************************************************************************)
(**	{1 Modules}								*)
(********************************************************************************)

module M : S =
struct
	type t =
		| Lambtex
		| Lambhtml
		| Lamblite

	let of_string = function
		| "lambtex"  -> Lambtex
		| "lambhtml" -> Lambhtml
		| "lamblite" -> Lamblite
		| _	     -> invalid_arg "Markup.of_string"

	let to_string = function
		| Lambtex  -> "lambtex"
		| Lambhtml -> "lambhtml"
		| Lamblite -> "lamblite"
end


(********************************************************************************)
(**	{1 Main}								*)
(********************************************************************************)

include M	(* For convenience sake we include the defined module *)

