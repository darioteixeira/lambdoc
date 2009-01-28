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

type raw_t = string (*with sexp*)			(** The type of raw text. *)

type plain_t = string (*with sexp*)			(** The type of undecorated text. *)

type entity_t = string (*with sexp*)		(** The type of XHTML entities. *)

type link_t = string (*with sexp*)			(** The type of links. *)

type alias_t = string (*with sexp*)			(** The type of aliases to filenames. *)

type ref_t = string (*with sexp*)			(** The type of label references. *)

type syntax_t = string option (*with sexp*)		(** The type for syntax declarations. *)

type tag_t = string (*with sexp*)			(** The type used for tags. *)

type 'a plus_t = 'a * 'a list (*with sexp*)		(** The type of non-empty lists. *)


(**     Definition of hierarchy levels for sections.  We support a three-level
	hierarchy, equivalent to XHTML's H1, H2, and H3.  These can be interpreted
	as "section", "subsection", and "subsubsection". 
*)
type hierarchical_level_t =
        [ `Level1
        | `Level2
        | `Level3
        ] (*with sexp*)


(**     Definition of hierarchy levels for titles.  We support a two-level
	hierarchy, equivalent to XHTML's H1 and H2.  These can be interpreted
	as "title" and "subtitle".
*)
type title_level_t =
	[ `Level1
	| `Level2
        ] (*with sexp*)


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

let fplus f elem elem_list = (f elem, List.map f elem_list)

