open Lambdoc

class processor : Netplex_types.processor =

let empty_hooks = new Netplex_kit.empty_processor_hooks () in

object (self)

	inherit Netplex_kit.processor_base empty_hooks

	method process ~when_done container fd proto_name =
		let in_channel = Unix.in_channel_of_descr fd
		and out_channel = Unix.out_channel_of_descr fd in
		Unix.clear_nonblock fd;
		let request : Parserver_protocol.request_t = Marshal.from_channel in_channel in
		(match request with
		| Parserver_protocol.Convert_manuscript_from_source source ->
			let manuscript = Lambdoc_io.manuscript_from_source source in
			Marshal.to_channel out_channel manuscript []
		| Parserver_protocol.Convert_composition_from_source source ->
			let composition = Lambdoc_io.composition_from_source source in
			Marshal.to_channel out_channel composition []);
		close_out out_channel;
		when_done ()

	method supported_ptypes = [ `Multi_processing; `Multi_threading ]
end


class processor_factory : Netplex_types.processor_factory =

object (self)

	method name = "converter"

	method create_processor ctrl_cfg cfg_file cfg_addr = new processor
end

