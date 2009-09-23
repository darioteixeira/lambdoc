open Lambdoc_reader

let doc =
	let src = IO.read_all (IO.input_channel (open_in "sample.ltex")) in
	let accept_list = [ ] in
	let deny_list = [ ] in
	let default = `Accept
	in Lambdoc_read_lambtex.Main.ambivalent_manuscript_from_string ~accept_list ~deny_list ~default src


let sexp_pickle = Lambdoc_core.Ambivalent.serialize_manuscript doc
let marshal_pickle = Marshal.to_string doc []

let conv_to_sexp () = let _ = Lambdoc_core.Ambivalent.serialize_manuscript doc in ()
let conv_from_sexp () = let _ = Lambdoc_core.Ambivalent.deserialize_manuscript sexp_pickle in ()

let conv_to_marshal () = let _ = Marshal.to_string doc [] in ()
let conv_from_marshal () = let _ = Marshal.from_string marshal_pickle 0 in ()


let tests =
	let funcs =
		[
		(conv_to_sexp, "serialisation with Sexp");
		(conv_from_sexp, "deserialisation with Sexp");
		(conv_to_marshal, "serialisation with Marshal");
		(conv_from_marshal, "deserialisation with Marshal")
		] in
	let test (func, msg) =
		let () = Gc.compact () in
		let time1 = Unix.gettimeofday () in
		let () = for i = 1 to 1000 do func () done in
		let time2 = Unix.gettimeofday () in
		Printf.printf "1000-fold %s took %.3f secs\n%!" msg (time2 -. time1)
	in List.iter test funcs

