(********************************************************************************)
(*	Labelmap.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	The label map contains a mapping between the labels used in the document
	and the ordering of the corresponding block.  Note that all labels share
	the same namespace.  Users are therefore encouraged to use the informal
	LaTeX convention of prefixing each label with [fig:], [tab:], [sec:], etc.
*)

(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type key_t = Label.t with sexp
type value_t = Target.t with sexp
type t with sexp


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

val create: unit -> t
val add: t -> key_t -> value_t -> unit
val mem: t -> key_t -> bool
val find: t -> key_t -> value_t

