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
    include String

    let strip ?(chars = " \t\r\n") str =
        let len = length str in
        let lidx = ref 0 in
        while !lidx < len && contains chars (unsafe_get str !lidx) do
            incr lidx
        done;
        let ridx = ref (len - 1) in
        while !ridx >= !lidx && contains chars (unsafe_get str !ridx) do
            decr ridx
        done;
        sub str !lidx (!ridx - !lidx + 1)

    let lstrip ?(chars = " \t\r\n") str =
        let len = length str in
        let lidx = ref 0 in
        while !lidx < len && contains chars (unsafe_get str !lidx) do
            incr lidx;
        done;
        sub str !lidx (len - !lidx)

    let rstrip ?(chars = " \t\r\n") str =
        let len = length str in
        let ridx = ref (len - 1) in
        while !ridx >= 0 && contains chars (unsafe_get str !ridx) do
            decr ridx;
        done;
        sub str 0 (!ridx + 1)

    let chop ?(left = 0) ?(right = 0) str =
        if left < 0 || right < 0 then invalid_arg "String.chop: negative count";
        let len = length str - left - right in
        if len > 0
        then sub str left len
        else ""

    let nsplit_by_char str sep =
        let len = length str in
        let rec loop idx count accum =
            if idx < 0
            then
                sub str (idx + 1) count :: accum
            else
                if unsafe_get str idx = sep
                then
                    let accum = sub str (idx + 1) count :: accum in
                    loop (idx - 1) 0 accum
                else
                    loop (idx - 1) (count + 1) accum in
        if len = 0
        then []
        else loop (len - 1) 0 []

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

    let replace_chars f str =
        let len = length str in
        let buf = Buffer.create len in
        for i = 0 to len - 1 do
            Buffer.add_string buf (f (unsafe_get str i))
        done;
        Buffer.contents buf

    let starts_with str prefix =
        let slen = length str in
        let plen = length prefix in
        let rec loop idx =
            if idx >= plen
            then
                true
            else
                let sx = unsafe_get str idx in
                let px = unsafe_get prefix idx in
                if sx = px
                then loop (idx + 1)
                else false in
        if plen > slen
        then false
        else loop 0
end

