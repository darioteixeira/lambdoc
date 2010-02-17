(********************************************************************************)
(*	Basic.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of the basic data types used in documents.
*)

TYPE_CONV_PATH "Basic"


(********************************************************************************)
(**	{1 Submodule definitions}						*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Ustring module}							*)
(********************************************************************************)

(**	We use Ocaml's native [string] to represent Unicode strings.
*)
module Ustring =
struct
	type t = string with sexp
end


(********************************************************************************)
(**	{2 Entity module}							*)
(********************************************************************************)

(**	HTML entities are represented as strings.
*)
module Entity =
struct
	type t = string with sexp
end


(********************************************************************************)
(**	{2 Uri module}								*)
(********************************************************************************)

(**	The type of links to external resources.
*)
module Uri =
struct
	type t = string with sexp
end


(********************************************************************************)
(**	{2 Classname module}							*)
(********************************************************************************)

(**	The type of classnames.
*)
module Classname =
struct
	type t = string with sexp
end


(********************************************************************************)
(**	{2 Alias module}							*)
(********************************************************************************)

(**	The type of aliases to filenames.
*)
module Alias =
struct
	type t = string with sexp
end


(********************************************************************************)
(**	{2 Ident module}							*)
(********************************************************************************)

(**	The type of identifiers.
*)
module Ident =
struct
	type t = string with sexp
end


(********************************************************************************)
(**	{2 Ref module}								*)
(********************************************************************************)

(**	The type of internal references.
*)
module Ref =
struct
	type t = string with sexp
end


(********************************************************************************)
(**	{2 Level module}							*)
(********************************************************************************)

(**	Definition of the levels used in documents.
*)
module Level =
struct
	(**     Definition of hierarchy levels for sections.  We support a
		three-level hierarchy, equivalent to XHTML's H1, H2, and H3.
		These can be interpreted as "section", "subsection", and
		"subsubsection". 
	*)
	type hierarchical_t =
		[ `Level1
		| `Level2
		| `Level3
		] with sexp


	(**     Definition of hierarchy levels for titles.  We support a
		two-level hierarchy, equivalent to XHTML's H1 and H2.
		These can be interpreted as "title" and "subtitle".
	*)
	type title_t =
		[ `Level1
		| `Level2
		] with sexp
end


(********************************************************************************)
(**	{2 Bullet module}							*)
(********************************************************************************)

(**	The various sorts of bullets accepted for unordered lists.
	Note that these map directly into their CSS counterparts.
*)
module Bullet =
struct
	type t =
		| Disc
		| Circle
		| Square
		| None
		with sexp
end


(********************************************************************************)
(**	{2 Numbering module}							*)
(********************************************************************************)

(**	The various sorts of numbering accepted for ordered lists.
	Note that these map directly into their CSS counterparts.
*)
module Numbering =
struct
	type t =
		| Decimal
		| Lower_roman
		| Upper_roman
		| Lower_alpha
		| Upper_alpha
		| None
		with sexp
end


(********************************************************************************)
(**	{2 Floatation module}							*)
(********************************************************************************)

(**	Definition of possible floatation options for document blocks.
*)
module Floatation =
struct
	type t =
		| Center
		| Left
		| Right
		with sexp
end

