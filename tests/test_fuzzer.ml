(********************************************************************************)
(*  Test_fuzzer.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Cmdliner
open Lambdoc_prelude


(********************************************************************************)
(** {1 Private modules}                                                    		*)
(********************************************************************************)

module List =
struct
    include List

    let init n f =
        let rec loop accum = function
            | 0 ->
                accum
            | i ->
                let next = i - 1 in
                let accum' = f next :: accum in
                loop accum' next in
        loop [] n
end

module Markup =
struct
    type t = Lambtex | Lambwiki | Lambxml | Markdown

	let all = [Lambtex; Lambwiki; Lambxml; Markdown]

	let to_string = function
		| Lambtex  -> "Lambtex"
		| Lambwiki -> "Lambwiki"
		| Lambxml  -> "Lambxml"
		| Markdown -> "Markdown"

    let extension = function
        | Lambtex  -> "tex"
        | Lambwiki -> "wiki"
        | Lambxml  -> "xml"
        | Markdown -> "md"

    let reader = function
        | Lambtex  -> Lambdoc_rlambtex_reader.Trivial.ambivalent_from_string ~options:()
        | Lambwiki -> Lambdoc_rlamblite_reader.Trivial.ambivalent_from_string ~options:`Lambwiki
        | Lambxml  -> Lambdoc_rlambxml_reader.Trivial.ambivalent_from_string ~options:()
        | Markdown -> Lambdoc_rlamblite_reader.Trivial.ambivalent_from_string ~options:`Markdown

    let parser x = match String.lowercase x with
		| "lambtex" | "tex"   -> `Ok Lambtex
		| "lambwiki" | "wiki" -> `Ok Lambwiki
		| "lambxml" | "xml"   -> `Ok Lambxml
		| "markdown" | "md"   -> `Ok Markdown
		| _                   -> `Error (Printf.sprintf "Unknown input markup '%s'" x)

	let printer fmt x =
		Format.pp_print_string fmt (to_string x)

    let converter = (parser, printer)
end


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

let progress =
    let spinner_chars = "-\\|/" in
    let len = String.length spinner_chars in
    let mutex = Mutex.create () in
    let counter = ref (-1) in
    let spinner = ref (-1) in
    fun status ->
        Mutex.lock mutex;
        begin match status with
            | `Success ->
                counter := (!counter + 1) mod 20;   (* Don't spin on every single invocation! *)
                if !counter = 0
                then begin
                    if !spinner >= 0 then Printf.printf "\b%!";
                    spinner := (!spinner + 1) mod len;
                    Printf.printf "%c%!" spinner_chars.[!spinner]
                end
            | `Failure ->
                if !spinner >= 0 then Printf.printf "\b%!";
                spinner := -1;
                Printf.printf "x%!"
            | `Cycle ->
                if !spinner >= 0 then Printf.printf "\b%!";
                spinner := -1;
                Printf.printf ".%!"
        end;
        Mutex.unlock mutex

let random_char () =
    Char.chr (Random.int 128)

let mutate orig =
    let src = Bytes.unsafe_of_string orig in (* This is safe: src is never mutated *)
    let len = Bytes.length src in
    match Random.int 4 with
        | 0 -> (* Insertion *)
            let dst = Bytes.create (len + 1) in
            let offset = Random.int (len + 1) in
            Bytes.blit src 0 dst 0 offset;
            Bytes.blit src offset dst (offset + 1) (len - offset);
            Bytes.set dst offset @@ random_char ();
            Bytes.unsafe_to_string dst
        | 1 when len >= 1 -> (* Deletion *)
            let dst = Bytes.create (len - 1) in
            let offset = Random.int len in
            Bytes.blit src 0 dst 0 offset;
            Bytes.blit src (offset + 1) dst offset (len - offset - 1);
            Bytes.unsafe_to_string dst
        | 2 when len >= 1 -> (* Substitution *)
            let dst = Bytes.copy src in
            let offset = Random.int len in
            Bytes.set dst offset @@ random_char ();
            Bytes.unsafe_to_string dst
        | 3 when len >= 2 -> (* Transposition *)
            let dst = Bytes.copy src in
            let offset = Random.int (len - 1) in
            let a = Bytes.get dst offset in
            let b = Bytes.get dst (offset + 1) in
            Bytes.set dst offset b;
            Bytes.set dst (offset + 1) a;
            Bytes.unsafe_to_string dst
        | n ->
            orig

let read_file basename ext =
    let fname = Printf.sprintf "%s.%s" basename ext in
    let chan = Pervasives.open_in fname in
    let str = Pervasives.input_all chan in
    Pervasives.close_in chan;
    str

let write_file basename ext str =
    let fname = Printf.sprintf "%s.%s" basename ext in
	let chan = Pervasives.open_out fname in
	Pervasives.output_string chan str;
	Pervasives.close_out chan

let execute_with_timeout reader str =
	let success = ref true in
	let executer () =
		let on_alarm _ =
			success := false;
			Thread.exit () in
		Sys.(set_signal sigalrm (Signal_handle on_alarm));
		ignore (Unix.alarm 1);
		(try ignore (reader str) with _ -> success := false);
		Sys.(set_signal sigalrm Signal_ignore) in
	let child = Thread.create executer () in
	Thread.join child;
	!success

let benchmark =
    let mutex = Mutex.create () in
    fun reader ext str ->
        Mutex.lock mutex;
		let t0 = Unix.gettimeofday () in
        let result =
			if execute_with_timeout reader str
			then begin
                progress `Success;
				let t1 = Unix.gettimeofday () in
				let len = String.length str in
				Some ((1_000_000.0 /. float_of_int len) *. ((1000.0 *. (t1 -. t0)) ** 2.0))
			end
			else begin
                progress `Failure;
                let basename = Printf.sprintf "error-%s" Digest.(string str |> to_hex) in
				write_file basename ext str;
                None
			end in
        Mutex.unlock mutex;
        result

let rec iterate num_iterations num_mutants reader ext src =
    let rec loop counter src =
        let mutants = List.init num_mutants (fun _ -> mutate src) in
        let results = List.map (benchmark reader ext) mutants in
        let f (best_mutant, best_result) mutant result = match (best_result, result) with
            | (Some b, Some r) when r > b -> (Some mutant, result)
            | (None, Some r)              -> (Some mutant, result)
            | _                           -> (best_mutant, best_result) in
        let (best_mutant, best_result) = List.fold_left2 f (None, None) mutants results in
        match best_mutant with
            | Some mutant when counter < num_iterations -> loop (counter + 1) mutant
            | _                                         -> () in
    loop 1 src

let fuzz sample num_cycles num_iterations num_mutants markup =
    let ext = Markup.extension markup in
    let reader = Markup.reader markup in
    let src = read_file sample ext in
    for i = 1 to num_cycles do
        iterate num_iterations num_mutants reader ext src;
        progress `Cycle;
        Gc.compact ()
    done

let main sample num_cycles num_iterations num_mutants markups =
    Random.self_init ();
    Printf.printf "Running fuzzer with cycles=%d, iterations=%d, and mutants=%d.\n" num_cycles num_iterations num_mutants;
    Printf.printf "Markups to be tested are [%s].\n" (List.map Markup.to_string markups |> String.concat "; ");
    Printf.printf "Sample files have basename '%s'.\n" sample;
    Printf.printf "Each dot marks the end of a cycle.\n";
    Printf.printf "Failure cases are marked with a 'x' and written to a file in the current directory.\n%!";
    let threads = List.map (Thread.create (fuzz sample num_cycles num_iterations num_mutants)) markups in
    List.iter Thread.join threads

let () =
    let sample =
        let doc = "Basename (ie, without extension) of the source file." in
        Arg.(value @@ opt string "sample" @@ info ~docv:"SAMPLE" ~doc ["s"; "sample"]) in
    let num_cycles =
        let doc = "Number of cycles per markup." in
        Arg.(value @@ opt int 10 @@ info ~docv:"CYCLES" ~doc ["c"; "cycles"]) in
    let num_iterations =
        let doc = "Number of mutation iterations per cycle." in
        Arg.(value @@ opt int 100 @@ info ~docv:"ITERATIONS" ~doc ["i"; "iterations"]) in
    let num_mutants =
        let doc = "Number of mutants per iteration." in
        Arg.(value @@ opt int 10 @@ info ~docv:"MUTANTS" ~doc ["m"; "mutants"]) in
    let markups =
        let doc = "Markups to be tested." in
        Arg.(value @@ opt (list Markup.converter) Markup.all @@ info ~docv:"MARKUPS" ~doc ["markups"]) in
	let term = Term.(const main $ sample $ num_cycles $ num_iterations $ num_mutants $ markups) in
	let info =
		let doc = "Fuzzing-based test of the Lambdoc library." in
		let man =
            [
            `S "BUGS";
            `P "Please report any issues at $(b,https://github.com/darioteixeira/lambdoc/issues)";
            `S "AUTHOR";
            `P "Dario Teixeira <dario.teixeira@nleyten.com>";
            ] in
		Term.info "test_fuzzer" ~version:Meta.version ~doc ~man in
	match Term.eval (term, info) with
		| `Error _ -> exit 1
		|  _       -> exit 0

