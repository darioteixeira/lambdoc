(********************************************************************************)
(*	Interface file for Idiosyncrasies module.

	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core


(********************************************************************************)
(**	{2 Type definitions}							*)
(********************************************************************************)

type t


(********************************************************************************)
(**	{2 Public functions and values}						*)
(********************************************************************************)

val make_composition_idiosyncrasies:
	?accept_list: Features.composition_feature_t list ->
	?deny_list: Features.composition_feature_t list ->
	?default: Features.default_t ->
	unit -> t

val make_manuscript_idiosyncrasies:
	?accept_list: Features.manuscript_feature_t list ->
	?deny_list: Features.manuscript_feature_t list ->
	?default: Features.default_t ->
	unit -> t

val check_feature: Features.manuscript_feature_t -> t -> bool

