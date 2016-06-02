(********************************************************************************)
(*  Test_fuzzer.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core


(********************************************************************************)
(** {1 Private modules}                                                    		*)
(********************************************************************************)

module List =
struct
    include Lambdoc_prelude.List

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


(********************************************************************************)
(** {1 Type definitions}                                                    	*)
(********************************************************************************)

type markup = Lambtex | Lambwiki | Lambxml | Markdown


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

let progress =
    let spinner_chars = "-\\|/" in
    let len = String.length spinner_chars in
    let mutex = Mutex.create () in
    let spinner = ref (-1) in
    fun success ->
        Mutex.lock mutex;
        if !spinner >= 0 then Printf.printf "\b%!";
        if success
        then begin
            spinner := (!spinner + 1) mod len;
            Printf.printf "%c%!" spinner_chars.[!spinner]
        end
        else begin
            spinner := -1;
            Printf.printf ".%!"
        end;
        Mutex.unlock mutex

let get_markup_info = function
    | Lambtex  -> ("Lambtex", "tex", Lambdoc_rlambtex_reader.Trivial.ambivalent_from_string ~options:())
    | Lambwiki -> ("Lambwiki", "wiki", Lambdoc_rlamblite_reader.Trivial.ambivalent_from_string ~options:`Lambwiki)
    | Lambxml  -> ("Lambxml", "xml", Lambdoc_rlambxml_reader.Trivial.ambivalent_from_string ~options:())
    | Markdown -> ("Markdown", "md", Lambdoc_rlamblite_reader.Trivial.ambivalent_from_string ~options:`Markdown)

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
    let chan = open_in fname in
	let buf = Buffer.create 0xffff in
	let rec loop () = match input_line chan with
		| line ->
			Buffer.add_string buf line;
			Buffer.add_char buf '\n';
			loop ()
		| exception End_of_file ->
			close_in chan;
			Buffer.contents buf in
	loop ()

let write_file basename ext str =
    let fname = Printf.sprintf "%s.%s" basename ext in
	let chan = open_out fname in
	output_string chan str;
	close_out chan

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
                progress true;
				let t1 = Unix.gettimeofday () in
				let len = String.length str in
				Some ((1_000_000.0 /. float_of_int len) *. ((1000.0 *. (t1 -. t0)) ** 2.0))
			end
			else begin
                progress false;
                let basename = Printf.sprintf "error-%s" Digest.(string str |> to_hex) in
				write_file basename ext str;
                None
			end in
        Mutex.unlock mutex;
        result

let rec iterate counter reader ext src =
    let mutants = List.init 10 (fun _ -> mutate src) in
    let results = List.map (benchmark reader ext) mutants in
    let f (best_mutant, best_result) mutant result = match (best_result, result) with
        | (Some b, Some r) when r > b -> (Some mutant, result)
        | (None, Some r)              -> (Some mutant, result)
        | _                           -> (best_mutant, best_result) in
    let (best_mutant, best_result) = List.fold_left2 f (None, None) mutants results in
    match best_mutant with
        | Some mutant when counter < 100 -> iterate (counter + 1) reader ext mutant
        | _                              -> ()

let fuzzer markup =
    let (desc, ext, reader) = get_markup_info markup in
    let src = read_file "sample" ext in
    while true do
        iterate 0 reader ext src
    done

let () =
    Random.self_init ();
    Printf.printf "\nRunning fuzzer...\n";
    Printf.printf "Note that this program runs forever. Press Ctrl+C to abort.\n%!";
    Printf.printf "Every time a dot appears, the corresponding failure case is written to a file.\n%!";
    let threads = List.map (Thread.create fuzzer) [Lambtex; Lambwiki; Lambxml; Markdown] in
    List.iter Thread.join threads

