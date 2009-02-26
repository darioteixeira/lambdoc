open Lambdoc_reader


let doc =
	let src = IO.read_all (IO.input_channel (open_in "sample.ltex")) in
	let accept_list = [ ] in
	let deny_list = [ ] in
	let default = `Accept
	in Read_lambtex.Main.ambivalent_manuscript_from_string ~accept_list ~deny_list ~default src


let sexp_pickle = Lambdoc_core.Ambivalent.serialize_manuscript_to_sexp doc
let binprot_pickle = Lambdoc_core.Ambivalent.serialize_manuscript_to_binprot doc
let marshal_pickle = Marshal.to_string doc []

let conv_to_sexp () = Lambdoc_core.Ambivalent.serialize_manuscript_to_sexp doc
let conv_from_sexp () = Lambdoc_core.Ambivalent.deserialize_manuscript_from_sexp sexp_pickle

let conv_to_binprot () = Lambdoc_core.Ambivalent.serialize_manuscript_to_binprot doc
let conv_from_binprot () = Lambdoc_core.Ambivalent.deserialize_manuscript_from_binprot binprot_pickle

let conv_to_marshal () = Marshal.to_string doc []
let conv_from_marshal () = Marshal.from_string marshal_pickle 0


let benchmark func =
	let time1 = Unix.gettimeofday () in
	let () = for i = 1 to 100000 do func () done in
	let time2 = Unix.gettimeofday () in
	time2 -. time1


let tests =
	let t1 = benchmark conv_to_sexp in
	let t2 = benchmark conv_from_sexp in
	let t3 = benchmark conv_to_binprot in
	let t4 = benchmark conv_from_binprot in
	let t5 = benchmark conv_to_marshal in
	let t6 = benchmark conv_from_marshal
	in	Printf.printf "Serialisation with Sexp took %.3f secs\n" t1;
		Printf.printf "Deserialisation with Sexp took %.3f secs\n" t2;
		Printf.printf "Serialisation with Binprot took %.3f secs\n" t3;
		Printf.printf "Deserialisation with Binprot took %.3f secs\n" t4;
		Printf.printf "Serialisation with Marshal took %.3f secs\n" t5;
		Printf.printf "Deserialisation with Marshal took %.3f secs\n" t6

