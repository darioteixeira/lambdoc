(********************************************************************************)
(*  Benchmark_serialization.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_prelude
open Lambdoc_document


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

let doc =
    let chan = Pervasives.open_in "sample.tex" in
    let src = Pervasives.input_all chan in
    Pervasives.close_in chan;
    Lambdoc_rlambtex_reader.Trivial.ambivalent_from_string src

let sexp_pickle = Ambivalent.serialize doc

let marshal_pickle = Marshal.to_string doc []

let () =
	let tests =
		[
		("Serialization with Sexp", (fun () -> ignore (Ambivalent.serialize doc)), ());
		("Serialization with Marshal", (fun () -> ignore (Marshal.to_string doc [])), ());
		("Deserialization with Sexp", (fun () -> ignore (Ambivalent.deserialize sexp_pickle)), ());
		("Deserialization with Marshal", (fun () -> ignore (Marshal.from_string marshal_pickle 0)), ());
		] in
    ignore Benchmark.(throughputN ~min_count:100L 10 tests)

