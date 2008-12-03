(********************************************************************************)
(*	Implementation file for Document_variety.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definition of document varieties.  Documents come in two distinct varieties:
	manuscripts and compositions.  The former are full-fledged documents, which
	may contain internal links, a table of contents, a bibliography, etc.  The
	latter do not contain internal links nor any of the other elements that
	imply internal links, such as a table of contents, a bibliography, etc.
*)

TYPE_CONV_PATH "Document"


(********************************************************************************)
(**	{2 Variety module}							*)
(********************************************************************************)

module Variety:
sig
	type t =
		[ `Manuscript
		| `Composition
		] (*with sexp*)
end =
struct
	type t =
		[ `Manuscript
		| `Composition
		] (*with sexp*)
end

