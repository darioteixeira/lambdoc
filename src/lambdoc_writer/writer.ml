(********************************************************************************)
(*	Implementation file for Writer module.

	Copyright (c) 2007-2008 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Document writer.
*)

open Lambdoc_core


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

(**	The module type that all wannabe document writers must export.
*)
module type S =
sig
	type t

	val write_valid_manuscript: ?settings:Settings.t -> Valid.manuscript_t -> t
	val write_valid_composition: ?settings:Settings.t -> Valid.composition_t -> t

	val write_invalid_manuscript: Invalid.manuscript_t -> t
	val write_invalid_composition: Invalid.composition_t -> t

	val write_ambivalent_manuscript: ?settings:Settings.t -> Ambivalent.manuscript_t -> t
	val write_ambivalent_composition: ?settings:Settings.t -> Ambivalent.composition_t -> t
end

