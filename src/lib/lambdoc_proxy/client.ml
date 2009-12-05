(********************************************************************************)
(*	Client.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lwt
open Protocol
open Lambdoc_core


(********************************************************************************)
(*	{2 Exceptions}								*)
(********************************************************************************)

exception Proxy_communication_error


(********************************************************************************)
(*	{2 Private functions and values}					*)
(********************************************************************************)

let communicate request =
	Lwt.catch
		(fun () ->
			let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 9999)
			and sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
			Lwt_unix.connect sock addr >>= fun () ->
			let in_channel = Lwt_chan.in_channel_of_descr sock
			and out_channel = Lwt_chan.out_channel_of_descr sock in
			Lwt_chan.output_value out_channel request >>= fun () ->
			Lwt_chan.flush out_channel >>= fun () ->
			Lwt_chan.input_value in_channel >>= fun reply ->
			Lwt_unix.shutdown sock Unix.SHUTDOWN_ALL;
			Lwt_unix.close sock;
			Lwt.return reply)
		(function
			| Unix.Unix_error _ -> Lwt.fail Proxy_communication_error
			| exc		    -> Lwt.fail exc)


(********************************************************************************)
(*	{2 Public functions and values}						*)
(********************************************************************************)

let ambivalent_manuscript_from_string ?accept_list ?deny_list ?default markup source =
	let payload =
		{
		m_accept_list = accept_list;
		m_deny_list = deny_list;
		m_default = default;
		m_source = source;
		} in
	let request = match markup with
		| `Lambtex	-> Manuscript_from_lambtex payload
		| `Lambxml	-> Manuscript_from_lambtex payload
	in communicate request >>= fun (reply : Ambivalent.manuscript_t) ->
	Lwt.return reply


let ambivalent_composition_from_string ?accept_list ?deny_list ?default markup source =
	let payload =
		{
		c_accept_list = accept_list;
		c_deny_list = deny_list;
		c_default = default;
		c_source = source;
		} in
	let request = match markup with
		| `Lambtex	-> Composition_from_lambtex payload
		| `Lambxml	-> Composition_from_lambtex payload
	in communicate request >>= fun (reply : Ambivalent.composition_t) ->
	Lwt.return reply

