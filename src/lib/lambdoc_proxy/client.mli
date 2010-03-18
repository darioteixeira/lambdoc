(********************************************************************************)
(*	Client.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type socket_t =
	{
	sockaddr: Unix.sockaddr;
	sockdomain: Unix.socket_domain;
	socktype: Unix.socket_type;
	sockproto: int;
	}


(********************************************************************************)
(*	{1 Public functions and values}						*)
(********************************************************************************)

val ambivalent_manuscript_from_string:
	?verify_utf8: bool ->
	?expand_entities: bool ->
	?accept_list: Features.manuscript_feature_t list ->
	?deny_list: Features.manuscript_feature_t list ->
	?default: Features.default_t ->
	socket: socket_t ->
	markup: Protocol.markup_t ->
	string ->
	Lambdoc_core.Ambivalent.manuscript_t Lwt.t


val ambivalent_composition_from_string:
	?verify_utf8: bool ->
	?expand_entities: bool ->
	?accept_list: Features.composition_feature_t list ->
	?deny_list: Features.composition_feature_t list ->
	?default: Features.default_t ->
	socket: socket_t ->
	markup: Protocol.markup_t ->
	string ->
	Lambdoc_core.Ambivalent.composition_t Lwt.t

