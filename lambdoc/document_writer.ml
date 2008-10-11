(********************************************************************************)
(**	Document writer.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Document_valid
open Document_invalid
open Document_ambivalent


(********************************************************************************)
(*	{2 Public modules}							*)
(********************************************************************************)

(**	The module type that all wannabe document writers must export.
*)
module type S =
sig
	type t

	val write_valid_manuscript: Valid.manuscript_t -> t
	val write_valid_composition: Valid.composition_t -> t

	val write_invalid_manuscript: Invalid.manuscript_t -> t
	val write_invalid_composition: Invalid.composition_t -> t

	val write_ambivalent_manuscript: Ambivalent.manuscript_t -> t
	val write_ambivalent_composition: Ambivalent.composition_t -> t
end

