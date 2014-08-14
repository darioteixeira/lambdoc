(********************************************************************************)
(*	Qanda.mli
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions pertaining to Q&A environments.
*)


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type t =
	| New_questioner of Inline.seq_t option
	| New_answerer of Inline.seq_t option
	| Same_questioner
	| Same_answerer
	with sexp

