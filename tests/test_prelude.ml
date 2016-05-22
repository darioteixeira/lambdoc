(********************************************************************************)
(*  Test_prelude.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_prelude


(********************************************************************************)
(** {1 Tests for the Lambdoc_prelude.List module}                               *)
(********************************************************************************)

let test_make () =
    Alcotest.(check (list int)) "0" [] (List.make 0 1);
    Alcotest.(check (list int)) "1" [1] (List.make 1 1);
    Alcotest.(check (list int)) "2" [1; 1] (List.make 2 1);
    Alcotest.check_raises "3" (Invalid_argument "List.make: negative count") (fun () -> ignore (List.make (-1) 1))

let test_take () =
    Alcotest.(check (list int)) "0" [] (List.take 0 []);
    Alcotest.(check (list int)) "1" [] (List.take 0 [1]);
    Alcotest.(check (list int)) "2" [] (List.take 1 []);
    Alcotest.(check (list int)) "3" [1] (List.take 1 [1]);
    Alcotest.(check (list int)) "4" [1] (List.take 1 [1; 2]);
    Alcotest.(check (list int)) "5" [1; 2] (List.take 2 [1; 2]);
    Alcotest.(check (list int)) "6" [1; 2] (List.take 2 [1; 2; 3]);
    Alcotest.(check (list int)) "7" [1; 2] (List.take 2 [1; 2; 3; 4]);
    Alcotest.check_raises "8" (Invalid_argument "List.take: negative count") (fun () -> ignore (List.take (-1) []))

let test_at () =
    Alcotest.(check int) "0" 0 (List.at [0; 1; 2] 0);
    Alcotest.(check int) "1" 1 (List.at [0; 1; 2] 1);
    Alcotest.(check int) "2" 2 (List.at [0; 1; 2] 2);
    Alcotest.check_raises "3" (Invalid_argument "List.at: negative index") (fun () -> ignore (List.at [0; 1; 2] (-1)));
    Alcotest.check_raises "4" (Invalid_argument "List.at: list too short") (fun () -> ignore (List.at [0; 1; 2] 3))

let test_filter_map () =
    let f x =
        if x mod 2 = 0
        then Some (string_of_int x)
        else None in
    Alcotest.(check (list string)) "0" [] (List.filter_map f []);
    Alcotest.(check (list string)) "1" [] (List.filter_map f [1]);
    Alcotest.(check (list string)) "2" ["0"; "2"] (List.filter_map f [0; 2]);
    Alcotest.(check (list string)) "3" ["0"; "2"] (List.filter_map f [0; 1; 2; 3])

let list_set =
    [
    ("List.make", `Quick, test_make);
    ("List.take", `Quick, test_take);
    ("List.at", `Quick, test_at);
    ("List.filter_map", `Quick, test_filter_map);
    ]


(********************************************************************************)
(** {1 Tests for the Lambdoc_prelude.String module}                             *)
(********************************************************************************)

let test_strip () =
    Alcotest.(check string) "0" "hello" (String.strip "hello");
    Alcotest.(check string) "1" "hello" (String.strip "hello ");
    Alcotest.(check string) "2" "hello" (String.strip "hello  ");
    Alcotest.(check string) "3" "hello" (String.strip " hello");
    Alcotest.(check string) "4" "hello" (String.strip "  hello");
    Alcotest.(check string) "5" "hello" (String.strip " hello ");
    Alcotest.(check string) "6" "" (String.strip " ");
    Alcotest.(check string) "7" "" (String.strip "")

let test_lstrip () =
    Alcotest.(check string) "0" "hello" (String.lstrip "hello");
    Alcotest.(check string) "1" "hello " (String.lstrip "hello ");
    Alcotest.(check string) "2" "hello  " (String.lstrip "hello  ");
    Alcotest.(check string) "3" "hello" (String.lstrip " hello");
    Alcotest.(check string) "4" "hello" (String.lstrip "  hello");
    Alcotest.(check string) "5" "hello " (String.lstrip " hello ");
    Alcotest.(check string) "6" "" (String.lstrip " ");
    Alcotest.(check string) "7" "" (String.lstrip "")

let test_rstrip () =
    Alcotest.(check string) "0" "hello" (String.rstrip "hello");
    Alcotest.(check string) "1" "hello" (String.rstrip "hello ");
    Alcotest.(check string) "2" "hello" (String.rstrip "hello  ");
    Alcotest.(check string) "3" " hello" (String.rstrip " hello");
    Alcotest.(check string) "4" "  hello" (String.rstrip "  hello");
    Alcotest.(check string) "5" " hello" (String.rstrip " hello ");
    Alcotest.(check string) "6" "" (String.rstrip " ");
    Alcotest.(check string) "7" "" (String.rstrip "")

let test_chop () =
    Alcotest.(check string) "0" "abcd" (String.chop "abcd");
    Alcotest.(check string) "1" "bcd" (String.chop ~left:1 "abcd");
    Alcotest.(check string) "2" "abc" (String.chop ~right:1 "abcd");
    Alcotest.(check string) "3" "bc" (String.chop ~left:1 ~right:1 "abcd");
    Alcotest.(check string) "4" "" (String.chop ~left:2 ~right:2 "abcd");
    Alcotest.(check string) "5" "" (String.chop ~left:3 ~right:2 "abcd");
    Alcotest.check_raises "6" (Invalid_argument "String.chop: negative count") (fun () -> ignore (String.chop ~left:(-1) "abcd"));
    Alcotest.check_raises "7" (Invalid_argument "String.chop: negative count") (fun () -> ignore (String.chop ~right:(-1) "abcd"))

let test_nsplit_by_char () =
    Alcotest.(check (list string)) "0" [] (String.nsplit_by_char "" '\n');
    Alcotest.(check (list string)) "0" ["a"] (String.nsplit_by_char "a" '\n');
    Alcotest.(check (list string)) "0" ["a"; "b"] (String.nsplit_by_char "a\nb" '\n');
    Alcotest.(check (list string)) "0" [""; "a"; ""; "b"; ""] (String.nsplit_by_char "\na\n\nb\n" '\n')

let test_asplit () =
    Alcotest.(check (list string)) "0" [] (String.asplit "" |> Array.to_list);
    Alcotest.(check (list string)) "1" [""] (String.asplit "\n" |> Array.to_list);
    Alcotest.(check (list string)) "2" [""; ""] (String.asplit "\n\n" |> Array.to_list);
    Alcotest.(check (list string)) "3" ["hello"] (String.asplit "hello" |> Array.to_list);
    Alcotest.(check (list string)) "4" ["hello"; "world"] (String.asplit "hello\nworld" |> Array.to_list);
    Alcotest.(check (list string)) "5" ["hello"; "world"] (String.asplit "hello\nworld\n" |> Array.to_list);
    Alcotest.(check (list string)) "6" ["hello"; "world"; ""] (String.asplit "hello\nworld\n\n" |> Array.to_list);
    Alcotest.(check (list string)) "7" [""; "hello"; "world"] (String.asplit "\nhello\nworld" |> Array.to_list);
    Alcotest.(check (list string)) "8" [""; ""; "hello"; "world"] (String.asplit "\n\nhello\nworld" |> Array.to_list);
    Alcotest.(check (list string)) "9" ["hello"; ""; "world"] (String.asplit "hello\n\nworld" |> Array.to_list);
    Alcotest.(check (list string)) "10" ["hello"; ""; ""; "world"] (String.asplit "hello\n\n\nworld" |> Array.to_list)

let test_replace_chars () =
    Alcotest.(check string) "0" "" (String.replace_chars (function _ -> "X") "");
    Alcotest.(check string) "0" "aaaaa" (String.replace_chars (function _ -> "a") "hello");
    Alcotest.(check string) "0" "" (String.replace_chars (function _ -> "") "hello world");
    Alcotest.(check string) "0" "hello(SP)world" (String.replace_chars (function ' ' -> "(SP)" | x -> String.make 1 x) "hello world")

let test_starts_with () =
    Alcotest.(check bool) "0" true (String.starts_with "" "");
    Alcotest.(check bool) "1" true (String.starts_with "a" "");
    Alcotest.(check bool) "2" true (String.starts_with "a" "a");
    Alcotest.(check bool) "3" true (String.starts_with "ab" "a");
    Alcotest.(check bool) "4" true (String.starts_with "ab" "ab");
    Alcotest.(check bool) "5" false (String.starts_with "" "a");
    Alcotest.(check bool) "6" false (String.starts_with "a" "b");
    Alcotest.(check bool) "7" false (String.starts_with "ab" "ac")

let string_set =
    [
    ("String.strip", `Quick, test_strip);
    ("String.lstrip", `Quick, test_lstrip);
    ("String.rstrip", `Quick, test_rstrip);
    ("String.chop", `Quick, test_chop);
    ("String.nsplit_by_char", `Quick, test_nsplit_by_char);
    ("String.asplit", `Quick, test_asplit);
    ("String.replace_chars", `Quick, test_replace_chars);
    ("String.starts_with", `Quick, test_starts_with);
    ]


(********************************************************************************)
(** {1 Tests for the Lambdoc_prelude.String module}                             *)
(********************************************************************************)

let sets =
    [
    ("list_set", list_set);
    ("string_set", string_set);
    ]

let () =
    Alcotest.run "Lambdoc_prelude" sets

