(********************************************************************************)
(*  Benchmark.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

let () =
    let src_lambtex = BatIO.read_all (BatIO.input_channel (open_in "sample.lambtex")) in
    let tests =
        [
        ("Parsing with Lambtex", (fun src -> Lambdoc_rlambtex_reader.Trivial.ambivalent_from_string src), src_lambtex);
        ]
    in Benchmark.(latencyN ~min_cpu:0.9 100L tests |> tabulate)

