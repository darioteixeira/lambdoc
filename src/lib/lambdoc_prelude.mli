(********************************************************************************)
(*  Lambdoc_prelude.mli
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Extensions to OCaml's standard library.  Note that for the most part the functions
    defined in this module have the same name and signature as functions in Batteries.
*)

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

module String:
sig
    include module type of String

    val of_char: char -> string
    val strip: ?chars:string -> string -> string
    val lstrip: ?chars:string -> string -> string
    val rstrip: ?chars:string -> string -> string
    val nsplit: string -> by:string -> string list
    val asplit: string -> string array
    val replace_chars: (char -> string) -> string -> string
    val slice: ?first:int -> ?last:int -> string -> string
    val explode: string -> char list
    val implode: char list -> string
    val fold_left: ('a -> char -> 'a) -> 'a -> string -> 'a
    val starts_with: string -> string -> bool
end

