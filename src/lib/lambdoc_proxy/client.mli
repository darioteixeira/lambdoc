(********************************************************************************)
(*	Client.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core


(********************************************************************************)
(*	{2 Exceptions}								*)
(********************************************************************************)

exception Proxy_communication_error


(********************************************************************************)
(*	{2 Public functions and values}						*)
(********************************************************************************)

val ambivalent_manuscript_from_string:
	?classnames: string list ->
	?accept_list: Features.manuscript_feature_t list ->
	?deny_list: Features.manuscript_feature_t list ->
	?default: Features.default_t ->
	Protocol.markup_t ->
	string ->
	Lambdoc_core.Ambivalent.manuscript_t Lwt.t


val ambivalent_composition_from_string:
	?classnames: string list ->
	?accept_list: Features.composition_feature_t list ->
	?deny_list: Features.composition_feature_t list ->
	?default: Features.default_t ->
	Protocol.markup_t ->
	string ->
	Lambdoc_core.Ambivalent.composition_t Lwt.t

