(********************************************************************************)
(*	Implementation file for Document_level.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of hierarchy levels.  We support a three-level hierarchy,
	equivalent to XHTML's H1, H2, and H3.  For sections, these levels can
	be interpreted as "section", "subsection", and "subsubsection".
*)

TYPE_CONV_PATH "Document"


(********************************************************************************)
(**	{2 Level module}							*)
(********************************************************************************)

module Level:
sig
	type t =
		| Level1
		| Level2
		| Level3
		(*with sexp*)
end =
struct
	type t =
		| Level1
		| Level2
		| Level3
		(*with sexp*)
end

