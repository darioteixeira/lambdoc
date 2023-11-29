open Lambdoc_prelude


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

let build_test (desc, ext, reader) =
    let fname = "sample." ^ ext in
    let chan = Pervasives.open_in fname in
    let src = Pervasives.input_all chan in
    Pervasives.close_in chan;
    ("Parsing with " ^ desc, reader, src)

let () =
    Printf.printf "IMPORTANT NOTE: the inputs for each reader differ significantly in complexity.\n";
    Printf.printf "Therefore, these benchmarks cannot be used to compare one reader against another.\n";
    Printf.printf "They are nevertheless useful for comparing performance across compiler versions.\n\n";
    let test_inputs =
        [
        ("Lambtex", "tex", fun str -> Lambdoc_rlambtex_reader.Trivial.ambivalent_from_string ~options:() str);
        ("Lambwiki", "wiki", fun str -> Lambdoc_rlamblite_reader.Trivial.ambivalent_from_string ~options:`Lambwiki str);
        ("Lambxml", "xml", fun str -> Lambdoc_rlambxml_reader.Trivial.ambivalent_from_string ~options:() str);
        ("Markdown", "md", fun str -> Lambdoc_rlamblite_reader.Trivial.ambivalent_from_string ~options:`Markdown str);
        ] in
    let tests = List.map build_test test_inputs in
    ignore Benchmark.(throughputN ~min_count:100L 10 tests)

