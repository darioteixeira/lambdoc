open Lambdoc


(********************************************************************************)
(* Tokenizer test.								*)
(********************************************************************************)


let test_tokenizer () =
	let ch = open_in "samples/simple.lamb" in
        let lexbuf = Lexing.from_channel ch in
        let tokenizer = new Lambtex_reader_impl.Lambtex_tokenizer.tokenizer in
	let res = ref None in
	while match !res with
		| Some (Lambtex_reader_impl.Lambtex_parser.EOF _ )	-> false
		| _							-> true
	do
		let token = tokenizer#consume lexbuf in
		Printf.printf "## %s\n" (Lambtex_reader_impl.Lambtex_debugger.to_string token);
		res := Some token
	done


(********************************************************************************)
(* Parses and prints sample document.						*)
(********************************************************************************)

(*
let test_print () =
	let source = Std.input_all (open_in "samples/test.lamb") in
	let manuscript = Lambtex_reader.ambivalent_manuscript_from_string source in
	let xhtml = Xhtml_writer.ambivalent_manuscript_to_xhtml manuscript in
	let str = Xhtmlpretty.xhtml_list_print [xhtml]
	in print_endline str
*)

let test_bool () =
	let source = Std.input_all (open_in "samples/test.lamb") in
	let manuscript = Lambtex_reader.ambivalent_manuscript_from_string source in
	let _ = Xhtml_writer.write_ambivalent_manuscript manuscript in
	match manuscript with
		| `Valid valid		-> print_endline "Valid!"
		| `Invalid invalid	-> print_endline "Invalid!"


(********************************************************************************)
(* Benchmarks Marshal vs Sexplib.						*)
(********************************************************************************)
(*
let get_manuscript () =
	let str = Std.input_all (open_in "samples/complete.lamb") in
	let manuscript = Lambdoc_io.manuscript_from_source (`Lambtex str) in
	manuscript


let run_marshal manuscript () =
	let marshalled = Marshal.to_string manuscript [] in
	let unmarshalled : Manuscript.valid_t = Marshal.from_string marshalled 0 in
	ignore (unmarshalled)


let run_sexplib manuscript () =
	let str = Manuscript.serialize manuscript in
	let manuscript_new = Manuscript.deserialize str in
	ignore (manuscript_new)


let benchmark test =
	let start = Unix.gettimeofday () in
	for i = 1 to 1000 do
		test ()
	done;
	let finish = Unix.gettimeofday () in
	let duration = finish -. start in
	duration


let test_benchmark () =
	let manuscript = get_manuscript () in
	let duration_marshal = benchmark (run_marshal manuscript) in
	let duration_sexplib = benchmark (run_sexplib manuscript) in
	Printf.printf "Marshal: %f\n" duration_marshal;
	Printf.printf "Sexplib: %f\n" duration_sexplib
*)

(********************************************************************************)
(* Main programme.								*)
(********************************************************************************)

let () =
	test_tokenizer ()

