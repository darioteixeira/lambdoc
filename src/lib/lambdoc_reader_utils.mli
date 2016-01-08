(********************************************************************************)
(*  Lambdoc_reader_utils.mli
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module String:
sig
    include module type of BatString

    val lstrip: ?chars:string -> string -> string
    val rstrip: ?chars:string -> string -> string
    val asplit: string -> string array
end

