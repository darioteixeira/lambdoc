(********************************************************************************)
(*	Writer.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Document writer.
*)

open Lambdoc_core


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

(**	The module type that all wannabe document writers must export.
*)
module type S =
sig
	type t

	val write_ambivalent_manuscript: ?translations:Translations.t -> ?settings:Settings.t -> Ambivalent.manuscript_t -> t
	val write_ambivalent_composition: ?translations:Translations.t -> ?settings:Settings.t -> Ambivalent.composition_t -> t

	val write_valid_manuscript: ?translations:Translations.t -> ?settings:Settings.t -> Valid.manuscript_t -> t
	val write_valid_composition: ?translations:Translations.t -> ?settings:Settings.t -> Valid.composition_t -> t

	val write_invalid_manuscript: Invalid.manuscript_t -> t
	val write_invalid_composition: Invalid.composition_t -> t
end

