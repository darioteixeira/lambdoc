(********************************************************************************)
(*	Implementation file for Document_label.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions pertaining to document labels.
*)

TYPE_CONV_PATH "Document"

open Document_basic


(********************************************************************************)
(**	{2 Label module}							*)
(********************************************************************************)

(**	The [Label] module encapsulates label-related definitions.
*)
module Label =
struct
	(**	Label identifiers can either be [`Auto_label] (when they're automatically
		specified by the system) or [`User_label] (when they're manually attributed
		by the user).
	*)
	type t =
		[ `Auto_label of ref_t
		| `User_label of ref_t
		] (*with sexp*)
end

