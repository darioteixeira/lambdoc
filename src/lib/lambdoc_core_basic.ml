(********************************************************************************)
(*	Lambdoc_core_basic.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core


(********************************************************************************)
(**	{1 Public modules}							*)
(********************************************************************************)

module Entity =
struct
	type t = string
end


module Href =
struct
	type t = string

	let compare = String.compare
end


module Classname =
struct
	type t = string
end


module Attr =
struct
	type t = Classname.t list

	let default = []
end


module Ident =
struct
	type t = string
end


module Extkey =
struct
	type t = int
end


module Pointer =
struct
	type t = string
end


module Level =
struct
	type section_t = int

	type title_t = int

	let max_section = 6

	let max_title = 2

	let section level =
		if level >= 1 && level <= max_section
		then level
		else invalid_arg ("Level.section: " ^ string_of_int level)

	let title level =
		if level >= 1 && level <= max_title
		then level
		else invalid_arg ("Level.title: " ^ string_of_int level)
end

