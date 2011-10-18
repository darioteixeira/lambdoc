(********************************************************************************)
(*	Protocol.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core


(********************************************************************************)
(*	{1 Type definitions}							*)
(********************************************************************************)

type manuscript_payload_t =
	{
	m_verify_utf8: bool option;
	m_expand_entities: bool option;
	m_accepted: Features.manuscript_feature_t list option;
	m_denied: Features.manuscript_feature_t list option;
	m_default: Features.default_t option;
	m_markup: Markup.t;
	m_source: string;
	}


type composition_payload_t =
	{
	c_verify_utf8: bool option;
	c_expand_entities: bool option;
	c_accepted: Features.composition_feature_t list option;
	c_denied: Features.composition_feature_t list option;
	c_default: Features.default_t option;
	c_markup: Markup.t;
	c_source: string;
	}


type request_t =
	| Request_manuscript of manuscript_payload_t
	| Request_composition of composition_payload_t


type reply_t =
	| Reply_manuscript of Ambivalent.manuscript_t
	| Reply_composition of Ambivalent.composition_t

