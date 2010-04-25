(********************************************************************************)
(*	Converter.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_proxy
open Markup
open Protocol


(********************************************************************************)
(**	{1 Classes}								*)
(********************************************************************************)

class processor : Netplex_types.processor =
let empty_hooks = new Netplex_kit.empty_processor_hooks () in
object (self)

	inherit Netplex_kit.processor_base empty_hooks

	method process ~when_done container fd proto_name =
		let in_channel = Unix.in_channel_of_descr fd
		and out_channel = Unix.out_channel_of_descr fd in
		Unix.clear_nonblock fd;
		let request : Protocol.request_t = Marshal.from_channel in_channel in
		let () = match request with
			| Protocol.Read_manuscript payload ->
				let verify_utf8 = payload.m_verify_utf8
				and expand_entities = payload.m_expand_entities
				and accept_list = payload.m_accept_list
				and deny_list = payload.m_deny_list
				and default = payload.m_default
				and source = payload.m_source
				and reader = match payload.m_markup with
					| Lambtex  -> Lambdoc_read_lambtex.Main.ambivalent_manuscript_from_string
					| Lambhtml -> Lambdoc_read_lambhtml.Main.ambivalent_manuscript_from_string
					| Lamblite -> Lambdoc_read_lamblite.Main.ambivalent_manuscript_from_string in
				let manuscript = reader ?verify_utf8 ?expand_entities ?accept_list ?deny_list ?default source
				in Marshal.to_channel out_channel manuscript []
			| Protocol.Read_composition payload ->
				let verify_utf8 = payload.c_verify_utf8
				and expand_entities = payload.c_expand_entities
				and accept_list = payload.c_accept_list
				and deny_list = payload.c_deny_list
				and default = payload.c_default
				and source = payload.c_source
				and reader = match payload.c_markup with
					| Lambtex  -> Lambdoc_read_lambtex.Main.ambivalent_composition_from_string
					| Lambhtml -> Lambdoc_read_lambhtml.Main.ambivalent_composition_from_string
					| Lamblite -> Lambdoc_read_lamblite.Main.ambivalent_composition_from_string in
				let composition = reader ?verify_utf8 ?expand_entities ?accept_list ?deny_list ?default source
				in Marshal.to_channel out_channel composition [] in
		let () = close_out out_channel
		in when_done ()

	method supported_ptypes = [ `Multi_processing; `Multi_threading ]
end


class processor_factory : Netplex_types.processor_factory =
object (self)
	method name = "converter"
	method create_processor ctrl_cfg cfg_file cfg_addr = new processor
end

