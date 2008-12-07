(********************************************************************************)
(*	Implementation file for References module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions pertaining to internal document references.
*)

TYPE_CONV_PATH "Document"


(**	The label dictionary contains a mapping between the labels used in the
	document and the ordering of the corresponding block.  Note that all
	labels share the same namespace.  Users are therefore encouraged to
	use the informal LaTeX convention of prefixing each label with [fig:],
	[tab:], [sec:], etc.
*)
type t = (Label.t, Target.t) Hashtbl.t (*with sexp*)

