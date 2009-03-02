open Lambdoc

let communicate request =
	let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 9999)
	and sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
	Unix.connect sock addr;
	let in_channel = Unix.in_channel_of_descr sock
	and out_channel = Unix.out_channel_of_descr sock in
	output_value out_channel request;
	flush out_channel;
	let reply : Manuscript.t = input_value in_channel in
	Unix.shutdown sock Unix.SHUTDOWN_ALL;
	Unix.close sock;
	match reply with
	| `Ok manuscript	-> true
	| `Error errors		-> false


let () =
	let in_ch = open_in "complete.lamb" in
	let request = Parserver_protocol.Convert_manuscript_from_source (`Lambtex (Std.input_all in_ch)) in
	for i = 1 to 10000 do
		Printf.printf "%i: %B %!" i (communicate request);
	done

