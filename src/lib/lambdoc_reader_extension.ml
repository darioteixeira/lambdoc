(********************************************************************************)
(*  Lambdoc_reader_extension.ml
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

module Ast = Lambdoc_reader_ast

open Lambdoc_core
open Basic


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type syntax =
    | Syn_empty
    | Syn_seq
    | Syn_lit
    | Syn_frag
    | Syn_raw of string
    | Syn_raw_raw of string * string
    | Syn_raw_seq of string
    | Syn_raw_seqopt of string

type extdef = ident * syntax


(********************************************************************************)
(** {1 Public signatures}                                                       *)
(********************************************************************************)

module type S =
sig
    module Monad: Monadic.S
    module Foldmapper: Foldmap.S with module Monad = Monad

    type ('a, 'b) result = [ `Okay of 'a | `Error of 'b ]
    type 'a function_result = ('a, Error.localized list) result
    type inline_function_result = (Ast.seq * Ast.frag) function_result Monad.t
    type block_function_result = (Ast.frag * Ast.frag) function_result Monad.t

    type inline_function =
        | Inlfun_empty of (Ast.command -> inline_function_result)
        | Inlfun_seq of (Ast.command -> Ast.seq -> inline_function_result)
        | Inlfun_raw of string * (Ast.command -> string -> inline_function_result)
        | Inlfun_raw_raw of string * string * (Ast.command -> string -> string -> inline_function_result)
        | Inlfun_raw_seq of string * (Ast.command -> string -> Ast.seq -> inline_function_result)
        | Inlfun_raw_seqopt of string * (Ast.command -> string -> Ast.seq option -> inline_function_result)

    type block_function =
        | Blkfun_empty of (Ast.command -> block_function_result)
        | Blkfun_seq of (Ast.command -> Ast.seq -> block_function_result)
        | Blkfun_lit of (Ast.command -> string -> block_function_result)
        | Blkfun_frag of (Ast.command -> Ast.frag -> block_function_result)
        | Blkfun_raw of string * (Ast.command -> string -> block_function_result)
        | Blkfun_raw_raw of string * string * (Ast.command -> string -> string -> block_function_result)

    type extcomm_def =
        | Inlextcomm of inline_function
        | Blkextcomm of block_function * Blkcat.t list

    type extcomm = ident * extcomm_def
end


(********************************************************************************)
(** {1 Public modules}                                                          *)
(********************************************************************************)

module Make (M: Monadic.S): S with module Monad = M =
struct
    module Monad = M
    module Foldmapper = Foldmap.Make (M)

    type ('a, 'b) result = [ `Okay of 'a | `Error of 'b ]
    type reader_result = (string, Error.msg list) result
    type 'a function_result = ('a, Error.localized list) result
    type link_reader = href -> reader_result option Monad.t
    type image_reader = href -> reader_result option Monad.t
    type inline_function_result = (Ast.seq * Ast.frag) function_result Monad.t
    type block_function_result = (Ast.frag * Ast.frag) function_result Monad.t

    type inline_function =
        | Inlfun_empty of (Ast.command -> inline_function_result)
        | Inlfun_seq of (Ast.command -> Ast.seq -> inline_function_result)
        | Inlfun_raw of string * (Ast.command -> string -> inline_function_result)
        | Inlfun_raw_raw of string * string * (Ast.command -> string -> string -> inline_function_result)
        | Inlfun_raw_seq of string * (Ast.command -> string -> Ast.seq -> inline_function_result)
        | Inlfun_raw_seqopt of string * (Ast.command -> string -> Ast.seq option -> inline_function_result)

    type block_function =
        | Blkfun_empty of (Ast.command -> block_function_result)
        | Blkfun_seq of (Ast.command -> Ast.seq -> block_function_result)
        | Blkfun_lit of (Ast.command -> string -> block_function_result)
        | Blkfun_frag of (Ast.command -> Ast.frag -> block_function_result)
        | Blkfun_raw of string * (Ast.command -> string -> block_function_result)
        | Blkfun_raw_raw of string * string * (Ast.command -> string -> string -> block_function_result)

    type extcomm_def =
        | Inlextcomm of inline_function
        | Blkextcomm of block_function * Blkcat.t list

    type extcomm = ident * extcomm_def
end


module Trivial = Make (Monadic.Identity)

