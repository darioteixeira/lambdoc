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
(**	{1 SOCKET module type}							*)
(********************************************************************************)

(**	This is the signature that any module given as parameter to the
	{!Make} functor should obey.  It basically defines the address
	and properties of the socket that we will create to communicate
	with the parsing server.
*)
module type SOCKET =
sig
	val sockaddr: unit -> Unix.sockaddr
	val sockdomain: unit -> Unix.socket_domain
	val socktype: unit -> Unix.socket_type
	val sockproto: unit -> int
end


(********************************************************************************)
(**	{1 S module type}							*)
(********************************************************************************)

(**	Signature of the module produced by the functor.
*)
module type S =
sig
	val ambivalent_manuscript_from_string:
		?verify_utf8: bool ->
		?expand_entities: bool ->
		?accept_list: Features.manuscript_feature_t list ->
		?deny_list: Features.manuscript_feature_t list ->
		?default: Features.default_t ->
		Protocol.markup_t ->
		string ->
		Lambdoc_core.Ambivalent.manuscript_t Lwt.t

	val ambivalent_composition_from_string:
		?verify_utf8: bool ->
		?expand_entities: bool ->
		?accept_list: Features.composition_feature_t list ->
		?deny_list: Features.composition_feature_t list ->
		?default: Features.default_t ->
		Protocol.markup_t ->
		string ->
		Lambdoc_core.Ambivalent.composition_t Lwt.t
end


(********************************************************************************)
(**	{1 Make module}								*)
(********************************************************************************)

(**	Functor that creates an instantiation of a proxy client.
*)
module Make (Socket: SOCKET): S =
struct
	let communicate request =
		let addr = Socket.sockaddr ()
		and sock = Lwt_unix.socket (Socket.sockdomain ()) (Socket.socktype ()) (Socket.sockproto ()) in
		Lwt_unix.connect sock addr >>= fun () ->
		let in_channel = Lwt_chan.in_channel_of_descr sock
		and out_channel = Lwt_chan.out_channel_of_descr sock in
		Lwt_chan.output_value out_channel request >>= fun () ->
		Lwt_chan.flush out_channel >>= fun () ->
		Lwt_chan.input_value in_channel >>= fun reply ->
		Lwt_unix.shutdown sock Unix.SHUTDOWN_ALL;
		Lwt_unix.close sock;
		Lwt.return reply


	let ambivalent_manuscript_from_string ?verify_utf8 ?expand_entities ?accept_list ?deny_list ?default markup source =
		let payload =
			{
			m_verify_utf8 = verify_utf8;
			m_expand_entities = expand_entities;
			m_accept_list = accept_list;
			m_deny_list = deny_list;
			m_default = default;
			m_markup = markup;
			m_source = source;
			} in
		communicate (Read_manuscript payload) >>= fun (reply : Ambivalent.manuscript_t) ->
		Lwt.return reply


	let ambivalent_composition_from_string ?verify_utf8 ?expand_entities ?accept_list ?deny_list ?default markup source =
		let payload =
			{
			c_verify_utf8 = verify_utf8;
			c_expand_entities = expand_entities;
			c_accept_list = accept_list;
			c_deny_list = deny_list;
			c_default = default;
			c_markup = markup;
			c_source = source;
			} in
		communicate (Read_composition payload) >>= fun (reply : Ambivalent.composition_t) ->
		Lwt.return reply
end

