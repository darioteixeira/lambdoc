(********************************************************************************)
(*  Lambdoc_prelude.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module List =
struct
    include List

    let make n x =
        if n < 0 then invalid_arg "List.make: negative count";
        let rec loop count accum =
            if count = 0
            then accum
            else loop (count - 1) (x :: accum) in
        loop n []

    let take n xs =
        if n < 0 then invalid_arg "List.take: negative count";
        let rec loop count = function
            | hd :: tl when count > 0 -> hd :: loop (count - 1) tl
            | _                       -> [] in
        loop n xs

    let at xs n =
        if n < 0 then invalid_arg "List.at: negative index";
        let rec loop count = function
            | hd :: _ when count = 0 -> hd
            | _ :: tl                -> loop (count - 1) tl
            | []                     -> invalid_arg "List.at: list too short" in
        loop n xs

    let filter_map f xs =
        let rec loop accum = function
            | hd :: tl ->
                begin match f hd with
                    | Some x -> loop (x :: accum) tl
                    | None   -> loop accum tl
                end
            | [] ->
                rev accum in
        loop [] xs
end


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

