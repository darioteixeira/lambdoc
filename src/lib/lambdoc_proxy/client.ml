(********************************************************************************)
(*	Client.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lwt
open Protocol
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
(**	{1 Private functions and values}					*)
(********************************************************************************)

let communicate socket request =
	let sock = Lwt_unix.socket socket.sockdomain socket.socktype socket.sockproto in
	Lwt_unix.connect sock socket.sockaddr >>= fun () ->
	let in_channel = Lwt_chan.in_channel_of_descr sock
	and out_channel = Lwt_chan.out_channel_of_descr sock in
	Lwt_chan.output_value out_channel request >>= fun () ->
	Lwt_chan.flush out_channel >>= fun () ->
	Lwt_chan.input_value in_channel >>= fun reply ->
	Lwt_unix.shutdown sock Unix.SHUTDOWN_ALL;
	Lwt_unix.close sock;
	Lwt.return reply


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

let ambivalent_manuscript_from_string ?verify_utf8 ?expand_entities ?accept ?deny ?default ~socket ~markup source =
	let payload =
		{
		m_verify_utf8 = verify_utf8;
		m_expand_entities = expand_entities;
		m_accept = accept;
		m_deny = deny;
		m_default = default;
		m_markup = markup;
		m_source = source;
		} in
	communicate socket (Read_manuscript payload) >>= fun (reply : Ambivalent.manuscript_t) ->
	Lwt.return reply


let ambivalent_composition_from_string ?verify_utf8 ?expand_entities ?accept ?deny ?default ~socket ~markup source =
	let payload =
		{
		c_verify_utf8 = verify_utf8;
		c_expand_entities = expand_entities;
		c_accept = accept;
		c_deny = deny;
		c_default = default;
		c_markup = markup;
		c_source = source;
		} in
	communicate socket (Read_composition payload) >>= fun (reply : Ambivalent.composition_t) ->
	Lwt.return reply

