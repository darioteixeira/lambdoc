(********************************************************************************)
(*	Parserver.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

let () =
	Netplex_log.debug_scheduling := false;
	Netplex_log.debug_containers := true;
	Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
	Blahcaml.init_dtd ();
	let (opt_list, cmdline_cfg) = Netplex_main.args () in
	let () = Arg.parse opt_list (fun s -> raise (Arg.Bad ("Don't know what to do with: " ^ s))) "usage: netplex [options]" in
	let parallelizer = Netplex_mp.mp ()
	in Netplex_main.startup
		parallelizer
		Netplex_log.logger_factories
		Netplex_workload.workload_manager_factories
		[new Converter.processor_factory]
		cmdline_cfg

