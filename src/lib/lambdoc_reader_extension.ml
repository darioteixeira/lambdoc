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

type syntax_t =
    | Syn_empty
    | Syn_seq
    | Syn_lit
    | Syn_frag
    | Syn_raw of string
    | Syn_raw_raw of string * string
    | Syn_raw_seq of string
    | Syn_raw_seqopt of string

type extdef_t = Ident.t * syntax_t


(********************************************************************************)
(** {1 Public signatures}                                                       *)
(********************************************************************************)

module type S =
sig
    module Monad: Monadic.S

    type ('a, 'b) result_t = [ `Okay of 'a | `Error of 'b ]
    type reader_result_t = (string, Error.msg_t list) result_t
    type 'a function_result_t = ('a, Error.localized_t list) result_t
    type link_reader_t = Href.t -> reader_result_t option Monad.t
    type image_reader_t = Href.t -> reader_result_t option Monad.t
    type inline_function_result_t = (Ast.seq_t * Ast.frag_t) function_result_t Monad.t
    type block_function_result_t = (Ast.frag_t * Ast.frag_t) function_result_t Monad.t

    type inline_function_t =
        | Inlfun_empty of (Ast.command_t -> inline_function_result_t)
        | Inlfun_seq of (Ast.command_t -> Ast.seq_t -> inline_function_result_t)
        | Inlfun_raw of string * (Ast.command_t -> string -> inline_function_result_t)
        | Inlfun_raw_raw of string * string * (Ast.command_t -> string -> string -> inline_function_result_t)
        | Inlfun_raw_seq of string * (Ast.command_t -> string -> Ast.seq_t -> inline_function_result_t)
        | Inlfun_raw_seqopt of string * (Ast.command_t -> string -> Ast.seq_t option -> inline_function_result_t)

    type block_function_t =
        | Blkfun_empty of (Ast.command_t -> block_function_result_t)
        | Blkfun_seq of (Ast.command_t -> Ast.seq_t -> block_function_result_t)
        | Blkfun_lit of (Ast.command_t -> string -> block_function_result_t)
        | Blkfun_frag of (Ast.command_t -> Ast.frag_t -> block_function_result_t)
        | Blkfun_raw of string * (Ast.command_t -> string -> block_function_result_t)
        | Blkfun_raw_raw of string * string * (Ast.command_t -> string -> string -> block_function_result_t)

    type extcomm_def_t =
        | Inlextcomm of inline_function_t
        | Blkextcomm of block_function_t * Blkcat.t list

    type extcomm_t = Ident.t * extcomm_def_t
end


(********************************************************************************)
(** {1 Public modules}                                                          *)
(********************************************************************************)

module Make (M: Monadic.S): S with module Monad = M =
struct
    module Monad = M

    type ('a, 'b) result_t = [ `Okay of 'a | `Error of 'b ]
    type reader_result_t = (string, Error.msg_t list) result_t
    type 'a function_result_t = ('a, Error.localized_t list) result_t
    type link_reader_t = Href.t -> reader_result_t option Monad.t
    type image_reader_t = Href.t -> reader_result_t option Monad.t
    type inline_function_result_t = (Ast.seq_t * Ast.frag_t) function_result_t Monad.t
    type block_function_result_t = (Ast.frag_t * Ast.frag_t) function_result_t Monad.t

    type inline_function_t =
        | Inlfun_empty of (Ast.command_t -> inline_function_result_t)
        | Inlfun_seq of (Ast.command_t -> Ast.seq_t -> inline_function_result_t)
        | Inlfun_raw of string * (Ast.command_t -> string -> inline_function_result_t)
        | Inlfun_raw_raw of string * string * (Ast.command_t -> string -> string -> inline_function_result_t)
        | Inlfun_raw_seq of string * (Ast.command_t -> string -> Ast.seq_t -> inline_function_result_t)
        | Inlfun_raw_seqopt of string * (Ast.command_t -> string -> Ast.seq_t option -> inline_function_result_t)

    type block_function_t =
        | Blkfun_empty of (Ast.command_t -> block_function_result_t)
        | Blkfun_seq of (Ast.command_t -> Ast.seq_t -> block_function_result_t)
        | Blkfun_lit of (Ast.command_t -> string -> block_function_result_t)
        | Blkfun_frag of (Ast.command_t -> Ast.frag_t -> block_function_result_t)
        | Blkfun_raw of string * (Ast.command_t -> string -> block_function_result_t)
        | Blkfun_raw_raw of string * string * (Ast.command_t -> string -> string -> block_function_result_t)

    type extcomm_def_t =
        | Inlextcomm of inline_function_t
        | Blkextcomm of block_function_t * Blkcat.t list

    type extcomm_t = Ident.t * extcomm_def_t
end


module Trivial = Make (Monadic.Identity)

