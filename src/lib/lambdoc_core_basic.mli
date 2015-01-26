(********************************************************************************)
(*	Lambdoc_core_basic.mli
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of the basic data types used in documents.
*)


(********************************************************************************)
(**	{1 Public modules}							*)
(********************************************************************************)

(**	HTML entities are represented as strings.
*)
module Entity:
sig
	type t = string with sexp
end


(**	The type of references to external resources.
*)
module Href:
sig
	type t = string with sexp
	val compare: t -> t -> int
end


(**	The type of classnames.
*)
module Classname:
sig
	type t = string with sexp
end


(**	The type of attributes that can be attached to inline and block elements.
*)
module Attr:
sig
	type t = Classname.t list with sexp

	val default: t
end


(**	The type of identifiers.
*)
module Ident:
sig
	type t = string with sexp
end


(**	The type of keys for extension commands.
*)
module Extkey:
sig
	type t = int with sexp
end


(**	The type of internal references.
*)
module Pointer:
sig
	type t = string with sexp
end


(**	Definition of the levels used in documents.
*)
module Level:
sig
	(**     Definition of hierarchy levels for sections.  We support a
		six-level hierarchy, equivalent to XHTML's H1 to H6.
	*)
	type section_t = private int with sexp

	(**     Definition of hierarchy levels for titles.  We support a
		two-level hierarchy, equivalent to XHTML's H1 and H2.
		These can be interpreted as "title" and "subtitle".
	*)
	type title_t = private int with sexp

	(**	Maximum accepted hierarchical level.
	*)
	val max_section: int

	(**	Maximum accepted title level.
	*)
	val max_title: int

	(**	Constructor for {!section_t}.  We force the use
		of this constructor my making the type private.
	*)
	val section: int -> section_t

	(**	Constructor for {!title_t}.  We force the use
		of this constructor my making the type private.
	*)
	val title: int -> title_t
end

