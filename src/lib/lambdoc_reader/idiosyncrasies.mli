(********************************************************************************)
(*	Idiosyncrasies.mli
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type t


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

val make_composition_idiosyncrasies:
	accept: Features.composition_feature_t list ->
	deny: Features.composition_feature_t list ->
	default: Features.default_t -> t

val make_manuscript_idiosyncrasies:
	accept: Features.manuscript_feature_t list ->
	deny: Features.manuscript_feature_t list ->
	default: Features.default_t -> t

val check_feature: Features.feature_t -> t -> bool

