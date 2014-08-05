(********************************************************************************)
(*	Fuzzer.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core

module String = BatString


let mutate str len = match Random.int 3 with
	| 0 ->	(* Insertion *)
		let x = String.make 1 (Char.chr (Random.int 128)) in
		let offset = Random.int (len + 1) in
		let str' = String.splice str offset 0 x in
		(str', len + 1)
	| 1 ->	(* Deletion *)
		let offset = Random.int len in
		let str' = String.splice str offset 1 "" in
		(str', len - 1)
	| 2 ->	(* Substitution *)
		let x = String.make 1 (Char.chr (Random.int 128)) in
		let offset = Random.int len in
		let str' = String.splice str offset 1 x in
		(str', len)
	| _ ->
		assert false


let iterate reader src len =
	let rec aux counter src len =
		let (src', len') = mutate src len in
		let is_valid =
			try match reader src' with
				| Ambivalent.Valid _   -> true
				| Ambivalent.Invalid _ -> false
			with exc ->
				let fname = Printf.sprintf "error-%f" (Unix.gettimeofday ()) in
				let chan = open_out (fname ^ ".exc") in
				Printexc.print_backtrace chan;
				close_out chan;
				let chan = open_out (fname ^ ".txt") in
				output_string chan src';
				close_out chan;
				raise exc in
		let counter = if is_valid then 0 else counter + 1 in
		Printf.printf "%c%!" (if is_valid then '.' else 'x');
		if counter < 20
		then aux counter src' len'
		else ()
	in aux 0 src len


let fuzz total markup src =
	let len = String.length src in
	let reader = match markup with
		| `Lambtex  -> Lambdoc_read_lambtex.Main.ambivalent_from_string
		| `Lambwiki -> Lambdoc_read_lambwiki.Main.ambivalent_from_string
		| `Lambxml  -> Lambdoc_read_lambxml.Main.ambivalent_from_string in
	for i = 1 to total do
		Printf.printf "#%03d: %!" i;
		let () = try iterate reader src len with exc -> print_char '!' in
		Printf.printf "\n%!"
	done


let () =
	Random.self_init ();
	Printexc.record_backtrace true;
	let src = BatPervasives.input_all stdin in
	fuzz 20 `Lambxml src

