(********************************************************************************)
(*  Benchmark.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

let build_test (desc, ext, reader) =
    let fname = "sample." ^ ext in
    let src = BatIO.read_all (BatIO.input_channel (open_in fname)) in
    ("Parsing with " ^ desc, reader, src)

let () =
    let test_inputs =
        [
        ("Lambtex", "tex", fun str -> Lambdoc_rlambtex_reader.Trivial.ambivalent_from_string ~options:() str);
        ("Lambwiki", "wiki", fun str -> Lambdoc_rlamblite_reader.Trivial.ambivalent_from_string ~options:`Lambwiki str);
        ("Lambxml", "xml", fun str -> Lambdoc_rlambxml_reader.Trivial.ambivalent_from_string ~options:() str);
        ("Markdown", "md", fun str -> Lambdoc_rlamblite_reader.Trivial.ambivalent_from_string ~options:`Markdown str);
        ] in
    let tests = List.map build_test test_inputs in
    Benchmark.(throughputN ~min_count:100L 10 tests |> tabulate)

