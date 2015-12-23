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
    let src_lambxml = BatIO.read_all (BatIO.input_channel (open_in "sample.lambxml")) in
    let tests =
        [
        ("Parsing with Lambtex", (fun src -> Lambdoc_rlambtex_reader.Trivial.ambivalent_from_string src), src_lambtex);
        ("Parsing with Lambxml", (fun src -> Lambdoc_rlambxml_reader.Trivial.ambivalent_from_string src), src_lambxml);
        ]
    in Benchmark.(throughputN ~min_count:100L 30 tests |> tabulate)

