(********************************************************************************)
(*	Basic.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Sexplib.Std


(********************************************************************************)
(**	{1 Public modules}							*)
(********************************************************************************)

module Entity =
struct
	type t = string with sexp
end


module Uri =
struct
	type t = string with sexp
end


module Classname =
struct
	type t = string with sexp
end


module Attr =
struct
	type t = Classname.t list with sexp

	let default = []
end


module Alias =
struct
	type t = string with sexp
end


module Ident =
struct
	type t = string with sexp
end


module Pointer =
struct
	type t = string with sexp
end


module Level =
struct
	type hierarchical_t = [ `Level1 | `Level2 | `Level3 ] with sexp

	type title_t = [ `Level1 | `Level2 ] with sexp
end

