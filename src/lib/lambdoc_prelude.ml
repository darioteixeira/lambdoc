(********************************************************************************)
(*  Lambdoc_prelude.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module List = BatList

module String =
struct
    include BatString

    let lstrip ?(chars = " \t\r\n") s =
        let p = ref 0 in
        let l = length s in
        while !p < l && contains chars (unsafe_get s !p) do
            incr p;
        done;
        sub s !p (l - !p)

    let rstrip ?(chars = " \t\r\n") s =
      let l = ref (length s - 1) in
      while !l >= 0 && contains chars (unsafe_get s !l) do
        decr l;
      done;
      sub s 0 (!l + 1)

    let asplit =
        let rex = Re.(compile (alt [char '\n'; str "\r\n"])) in
        fun str ->
            let (acc0, toks) = match Re.split_full rex str with
                | `Delim _ :: tl -> ((1, [""]), tl)
                | xs             -> ((0, []), xs) in
            let proc ((counter, lines) as acc) = function
                | `Text text -> (counter + 1, text :: lines)
                | `Delim _   -> acc in
            let (total, xs) = List.fold_left proc acc0 toks in
            let lines = Array.make total "" in
            let proc counter line =
                lines.(total - counter - 1) <- line;
                counter + 1 in
            let counter = List.fold_left proc 0 xs in
            assert (counter = total);
            lines
end

