(********************************************************************************)
(*	Implementation file for Client module.

	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lwt
open Protocol
open Lambdoc_core


(********************************************************************************)
(*	{2 Functions and values}						*)
(********************************************************************************)

let communicate request =
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
        Lwt.return reply


let manuscript_from_lambtex str =
	let request = Manuscript_from_lambtex str in
	communicate request >>= fun (reply : Ambivalent.manuscript_t) ->
	Lwt.return reply


let composition_from_lambtex str =
	let request = Composition_from_lambtex str in
	communicate request >>= fun (reply : Ambivalent.composition_t) ->
	Lwt.return reply

