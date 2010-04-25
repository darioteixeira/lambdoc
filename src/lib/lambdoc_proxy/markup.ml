(********************************************************************************)
(*	Markup.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

module type S =
sig
	type t =
		| Lambtex
		| Lambhtml
		| Lamblite
end

module M : S =
struct
	type t =
		| Lambtex
		| Lambhtml
		| Lamblite
end

include M

