(********************************************************************************)
(*	Protocol.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core


(********************************************************************************)
(*	{2 Type definitions}							*)
(********************************************************************************)

type markup_t =
	[ `Lambtex
	| `Lambxml
	]


type manuscript_payload_t =
	{
	m_classnames: string list option;
	m_accept_list: Features.manuscript_feature_t list option;
	m_deny_list: Features.manuscript_feature_t list option;
	m_default: Features.default_t option;
	m_source: string;
	}


type composition_payload_t =
	{
	c_classnames: string list option;
	c_accept_list: Features.composition_feature_t list option;
	c_deny_list: Features.composition_feature_t list option;
	c_default: Features.default_t option;
	c_source: string;
	}


type request_t =
	| Manuscript_from_lambtex of manuscript_payload_t
	| Composition_from_lambtex of composition_payload_t

