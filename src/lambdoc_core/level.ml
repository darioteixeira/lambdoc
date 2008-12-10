(********************************************************************************)
(*	Implementation file for Level module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Document levels.
*)

TYPE_CONV_PATH "Document"


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

(**	Definition of hierarchy levels.  We support a three-level hierarchy,
	equivalent to XHTML's H1, H2, and H3.  For sections, these levels can
	be interpreted as "section", "subsection", and "subsubsection".
*)
type t =
	| Level1
	| Level2
	| Level3
	(*with sexp*)
