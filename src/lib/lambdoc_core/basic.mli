(********************************************************************************)
(*	Basic.mli
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of the basic data types used in documents.
*)


(********************************************************************************)
(**	{1 Mdule definitions}							*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Entity module}							*)
(********************************************************************************)

(**	HTML entities are represented as strings.
*)
module Entity:
sig
	type t = string with sexp
end


(********************************************************************************)
(**	{2 Uri module}								*)
(********************************************************************************)

(**	The type of links to external resources.
*)
module Uri:
sig
	type t = string with sexp
end


(********************************************************************************)
(**	{2 Classname module}							*)
(********************************************************************************)

(**	The type of classnames.
*)
module Classname:
sig
	type t = string with sexp
end


(********************************************************************************)
(**	{2 Attribute module}							*)
(********************************************************************************)

(**	The type of attributes that can be attached to inline and block elements.
*)
module Attr:
sig
	type t = Classname.t list with sexp

	val default: t
end


(********************************************************************************)
(**	{2 Alias module}							*)
(********************************************************************************)

(**	The type of aliases to filenames.
*)
module Alias:
sig
	type t = string with sexp
end


(********************************************************************************)
(**	{2 Ident module}							*)
(********************************************************************************)

(**	The type of identifiers.
*)
module Ident:
sig
	type t = string with sexp
end


(********************************************************************************)
(**	{2 Pointer module}							*)
(********************************************************************************)

(**	The type of pointers (internal references).
*)
module Pointer:
sig
	type t = string with sexp
end


(********************************************************************************)
(**	{2 Level module}							*)
(********************************************************************************)

(**	Definition of the levels used in documents.
*)
module Level:
sig
	(**     Definition of hierarchy levels for sections.  We support a
		three-level hierarchy, equivalent to XHTML's H1, H2, and H3.
		These can be interpreted as "section", "subsection", and
		"subsubsection". 
	*)
	type hierarchical_t = [ `Level1 | `Level2 | `Level3 ] with sexp


	(**     Definition of hierarchy levels for titles.  We support a
		two-level hierarchy, equivalent to XHTML's H1 and H2.
		These can be interpreted as "title" and "subtitle".
	*)
	type title_t = [ `Level1 | `Level2 ] with sexp
end

