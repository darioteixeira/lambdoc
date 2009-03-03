let main () =

	(* Create a parser for the standard Netplex command-line arguments: *)
	let (opt_list, cmdline_cfg) = Netplex_main.args () in

	(* Parse the command-line arguments: *)
	Arg.parse
		opt_list
		(fun s -> raise (Arg.Bad ("Don't know what to do with: " ^ s)))
		"usage: netplex [options]";

	(* Select multi-processing: *)
	let parallelizer = Netplex_mp.mp () in  

	(* Start the Netplex system: *)
	Netplex_main.startup
		parallelizer
		Netplex_log.logger_factories
		Netplex_workload.workload_manager_factories
		[new Converter.processor_factory]
		cmdline_cfg
		;;

let () =
	Netplex_log.debug_scheduling := false;
	Netplex_log.debug_containers := false;
	Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
	main ()

