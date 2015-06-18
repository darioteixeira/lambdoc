(********************************************************************************)
(*  Serialisation_test.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Sexplib.Std
open Lambdoc_reader


(********************************************************************************)
(** {1 Functions and values}                                                    *)
(********************************************************************************)

let doc =
    let src = BatIO.read_all (BatIO.input_channel (open_in "sample.lambtex")) in
    Lambdoc_rlambtex_reader.Trivial.ambivalent_from_string src


let sexp_pickle = Lambdoc_core.Ambivalent.serialize doc
let marshal_pickle = Marshal.to_string doc []

let conv_to_sexp () = let _ = Lambdoc_core.Ambivalent.serialize doc in ()
let conv_from_sexp () = let _ = Lambdoc_core.Ambivalent.deserialize sexp_pickle in ()

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
        Printf.printf "Testing 1000-fold %s... %!" msg;
        let () = Gc.compact () in
        let time1 = Unix.gettimeofday () in
        let () = for i = 1 to 1000 do func () done in
        let time2 = Unix.gettimeofday () in
        Printf.printf "Required %.3f secs\n%!" (time2 -. time1)
    in List.iter test funcs

