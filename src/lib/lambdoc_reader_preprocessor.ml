(********************************************************************************)
(*  Lambdoc_reader_preprocessor.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)


(********************************************************************************)
(** {1 Private exceptions}                                                      *)
(********************************************************************************)

exception Malformed_code_point


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

(** This function verifies the input, making sure that any invalid sequence
    is replaced with the standard UTF-8 replacement character.  At the end,
    an exception is raised if any errors have been found.
*)
let verify_utf8 s =
    let line = ref 1 in
    let error_lines = ref [] in
    let buf = Buffer.create (String.length s) in
    let add_valid pos len =
        Buffer.add_substring buf s pos len in
    let add_invalid () =
        let () = match !error_lines with
            | hd :: _ when hd = !line -> ()         (* Only one error per line *)
            | _                       -> error_lines := !line :: !error_lines
        in Buffer.add_string buf "\xef\xbf\xbd" in  (* 0xfffd is the standard UTF-8 replacement character *)
    let rec trail c i a =
        if c = 0
        then a
        else if i >= String.length s
        then
            raise Malformed_code_point
        else
            let n = Char.code (String.unsafe_get s i) in
            if n < 0x80 || n >= 0xc0
            then raise Malformed_code_point
            else trail (c - 1) (i + 1) (a lsl 6 lor (n - 0x80)) in
    let rec main i =
        if i >= String.length s then Buffer.contents buf else
        let n = Char.code (String.unsafe_get s i) in
        if n < 0x80
        then    (* One-byte character *)
            let () = if n = 0x0a then incr (line) in    (* We expect lines to be terminated with '\r\n' or just '\n' *)
            (add_valid i 1; main (i + 1))
        else if n < 0xc2
        then
            (add_invalid (); main (i + 1))
        else if n <= 0xdf
        then    (* Two-byte character *)
            let () =
                try if trail 1 (i + 1) (n - 0xc0) < 0x80 then add_invalid () else add_valid i 2
                with Malformed_code_point -> add_invalid ()
            in main (i + 2)
        else if n <= 0xef
        then    (* Three-byte character *)
            let () =
                try if trail 2 (i + 1) (n - 0xe0) < 0x800 then add_invalid () else add_valid i 3
                with Malformed_code_point -> add_invalid ()
            in main (i + 3)
        else if n <= 0xf7
        then    (* Four-byte character *)
            let () =
                try if trail 3 (i + 1) (n - 0xf0) < 0x10000 then add_invalid () else add_valid i 4
                with Malformed_code_point -> add_invalid ()
            in main (i + 4)
        else if n <= 0xfb
        then    (* Five-byte character: invalid as per RFC 3629 *)
            (add_invalid (); main (i + 5))
        else if n <= 0xfd
        then    (* Six-byte character: invalid as per RFC 3629 *)
            (add_invalid (); main (i + 6))
        else
            (add_invalid (); main (i + 1))
    in
        let sane = main 0 in
        if !error_lines = []
        then `Okay
        else `Error (sane, List.rev !error_lines)

