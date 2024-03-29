module Int =
struct
    type t = int

    let compare (x: int) (y: int) =
        if x > y
        then 1
        else if y > x
        then -1
        else 0
end

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

module Monad =
struct
    module type S =
    sig
        type 'a t

        val return: 'a -> 'a t
        val fail: exn -> 'a t
        val bind: 'a t -> ('a -> 'b t) -> 'b t
        val catch: (unit -> 'a t) -> (exn -> 'a t) -> 'a t
        val fold_right: ('a -> 'b -> 'b t) -> 'a list -> 'b -> 'b t
    end

    module Identity: S with type 'a t = 'a =
    struct
        type 'a t = 'a

        let return x = x
        let fail exc = raise exc
        let bind t f = f t
        let catch f g = try f () with exc -> g exc
        let fold_right = List.fold_right
    end
end

module Pervasives =
struct
    include Pervasives

    let input_all chan =
        let bufsize = 65536 in
        let buf = Bytes.create bufsize in
        let rec read_loop pos accum =
            let len = input chan buf 0 bufsize in
            if len > 0
            then
                let hd = Bytes.sub buf 0 len in
                read_loop (pos + len) (hd :: accum)
            else
                (pos, accum) in
        let (size, accum) = read_loop 0 [] in
        let dst = Bytes.create size in
        let rec write_loop pos = function
            | hd :: tl ->
                let len = Bytes.length hd in
                let dstoff = pos - len in
                Bytes.blit hd 0 dst dstoff len;
                write_loop dstoff tl
            | [] ->
                Bytes.unsafe_to_string dst in
        write_loop size accum
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

    let nsplit_by_line str =
        let len = length str in
        let rec loop first current count accum =
            if current >= len
            then
                if current > first
                then (count + 1, sub str first (current - first) :: accum)
                else (count, accum)
            else
                let next = current + 1 in
                match unsafe_get str current with
                    | '\n' ->
                        let accum = sub str first (current - first) :: accum in
                        loop next next (count + 1) accum
                    | '\r' when next < len && unsafe_get str next = '\n' ->
                        let accum = sub str first (current - first) :: accum in
                        loop (next + 1) (next + 1) (count + 1) accum
                    | _ ->
                        loop first next count accum in
        let (count, xs) = loop 0 0 0 [] in
        let lines = Array.make count "" in
        let f i line = lines.(count - i - 1) <- line in
        List.iteri f xs;
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

