(********************************************************************************)
(*	Protocol.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core


(********************************************************************************)
(*	{1 Type definitions}							*)
(********************************************************************************)

type markup_t =
	| Lambtex
	| Lambhtml
	| Lamblite


type manuscript_payload_t =
	{
	m_verify_utf8: bool option;
	m_expand_entities: bool option;
	m_accept_list: Features.manuscript_feature_t list option;
	m_deny_list: Features.manuscript_feature_t list option;
	m_default: Features.default_t option;
	m_markup: markup_t;
	m_source: string;
	}


type composition_payload_t =
	{
	c_verify_utf8: bool option;
	c_expand_entities: bool option;
	c_accept_list: Features.composition_feature_t list option;
	c_deny_list: Features.composition_feature_t list option;
	c_default: Features.default_t option;
	c_markup: markup_t;
	c_source: string;
	}


type request_t =
	| Read_manuscript of manuscript_payload_t
	| Read_composition of composition_payload_t

