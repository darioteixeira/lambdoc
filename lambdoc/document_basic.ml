(********************************************************************************)
(**	Definition of the basic data types used in documents.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

TYPE_CONV_PATH "Document"


(********************************************************************************)
(**	{2 Basic data types used in the document}				*)
(********************************************************************************)

(**	The basic type of undecorated text.
*)
type plain_t = string with sexp

(**	The type of XHTML entities.
*)
type entity_t = string with sexp

(**	The type of links.
*)
type link_t = string with sexp

(**	The type of filenames.
*)
type filename_t = string with sexp

(**	The type of label references.
*)
type ref_t = string with sexp

(**	The type for syntax declarations.
*)
type syntax_t = string option with sexp

(**	The type used for tags.
*)
type tag_t = string with sexp

(**	The type of non-empty lists.
*)
type 'a plus_t = 'a * 'a list with sexp


(********************************************************************************)
(**	{2 Basic auxiliary functions}						*)
(********************************************************************************)

let fplus f elem elem_list = (f elem, List.map f elem_list)

