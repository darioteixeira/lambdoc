(** Extensions to OCaml's standard library.  Note that for the most part the functions
    defined in this module have the same name and signature as functions in Batteries.
*)

module Int:
sig
    type t = int

    val compare: int -> int -> int
end

module List:
sig
    include module type of List

    (** [make n x] creates a list of [n] elements, all of them [x]. *)
    val make: int -> 'a -> 'a list

    (** [take n xs] returns at least the first [n] elements from list [xs]. *)
    val take: int -> 'a list -> 'a list

    (** [at xs idx] returns the element at position [idx] in list [xs]. *)
    val at: 'a list -> int -> 'a

    (** [filter_map f xs] performs a simultaneous mapping and filtering on list [xs]. *)
    val filter_map: ('a -> 'b option) -> 'a list -> 'b list
end

module Monad:
sig
    module type S =
    sig
        type 'a t

        val return: 'a -> 'a t
        val fail: exn -> 'a t
        val bind: 'a t -> ('a -> 'b t) -> 'b t
        val catch: (unit -> 'a t) -> (exn -> 'a t) -> 'a t
        val fold_right: ('a -> 'b -> 'b t) -> 'a list -> 'b -> 'b t
    end

    module Identity: S with type 'a t = 'a
end

module Pervasives:
sig
    include module type of Pervasives

    (** [input_all chan] reads the entire contents of the input channel [chan],
        returning them as a string. *)
    val input_all: in_channel -> string
end

module String:
sig
    include module type of String

    (** [strip ?chars str] returns [str] with any of the characters in [chars] removed from
        both the beginning and end of the string.  [chars] defaults to " \t\r\n". *)
    val strip: ?chars:string -> string -> string

    (** [lstrip ?chars str] returns [str] with any of the characters in [chars] removed from
        the beginning of the string.  [chars] defaults to " \t\r\n". *)
    val lstrip: ?chars:string -> string -> string

    (** [rstrip ?chars str] returns [str] with any of the characters in [chars] removed from
        the end of the string.  [chars] defaults to " \t\r\n". *)
    val rstrip: ?chars:string -> string -> string

    (** [chop ?left ?right str] returns the string [str] without the leftmost [left] characters
        and the rightmost [right] characters.  Both [left] and [right] default to 0. *)
    val chop: ?left:int -> ?right:int -> string -> string

    (** [nsplit str sep] splits the string [str] at every separation character [sep],
        returning a list of all segments found. *)
    val nsplit_by_char: string -> char -> string list

    (** [nsplit_by_line str] splits the string [str] at every occurence of newlines (either '\n'
        or '\r\n'), returning an array of all segments found.  Note that empty lines are mapped
        to an empty string in the corresponding array position. *)
    val nsplit_by_line: string -> string array

    (** [replace_chars f str] replaces each character in string [str] by the result of applying
        function [f] on the character. *)
    val replace_chars: (char -> string) -> string -> string

    (** [starts_with str prefix] returns a boolean indicating whether the [str] starts with [prefix]. *)
    val starts_with: string -> string -> bool
end

