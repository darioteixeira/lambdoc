(********************************************************************************)
(*	Implementation file for Basic module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of the basic data types used in documents.
*)

TYPE_CONV_PATH "Document"


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

(**	The type of raw text.
*)
type raw_t = string (*with sexp*)

(**	The type of undecorated text.
*)
type plain_t = string (*with sexp*)

(**	The type of XHTML entities.
*)
type entity_t = string (*with sexp*)

(**	The type of links.
*)
type link_t = string (*with sexp*)

(**	The type of aliases to filenames.
*)
type alias_t = string (*with sexp*)

(**	The type of label references.
*)
type ref_t = string (*with sexp*)

(**	The type for syntax declarations.
*)
type syntax_t = string option (*with sexp*)

(**	The type used for tags.
*)
type tag_t = string (*with sexp*)

(**	The type of non-empty lists.
*)
type 'a plus_t = 'a * 'a list (*with sexp*)


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

let fplus f elem elem_list = (f elem, List.map f elem_list)

