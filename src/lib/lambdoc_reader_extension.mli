(********************************************************************************)
(*  Lambdoc_reader_extension.mli
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Reader extension.
*)

module Ast = Lambdoc_reader_ast

open Lambdoc_core
open Basic


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type syntax_t =
    | Syn_empty                         (* No main parameters. Eg: Linebreak, Appendix *)
    | Syn_seq                           (* Parameter is an inline sequence. Eg: Bold, Paragraph *)
    | Syn_lit                           (* Parameter is a multiline sequence of raw text. Eg: Verbatim *)
    | Syn_frag                          (* Parameter is a fragment (list of blocks). Eg: Quote *)
    | Syn_raw of string                 (* Parameter is single sequence of raw text. Eg: Macroarg *)
    | Syn_raw_raw of string * string    (* Parameters are two sequences of raw text. Eg: Glyph, Picture *)
    | Syn_raw_seq of string             (* Parameters are a sequence of raw text followed by an inline sequence. Eg: Mref *)
    | Syn_raw_seqopt of string          (* Parameters are a sequence of raw text optionally followed by an inline sequence. Eg: Link *)


type extdef_t = ident_t * syntax_t


(********************************************************************************)
(** {1 Public signatures}                                                       *)
(********************************************************************************)

(** The signature of a reader extension.
*)
module type S =
sig
    module Monad: Monadic.S
    module Foldmapper: Foldmap.S with module Monad = Monad

    type ('a, 'b) result_t = [ `Okay of 'a | `Error of 'b ]
    type 'a function_result_t = ('a, Error.localized_t list) result_t
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

    type extcomm_t = ident_t * extcomm_def_t
end


(********************************************************************************)
(** {1 Public modules}                                                          *)
(********************************************************************************)

module Make: functor (M: Monadic.S) -> S with module Monad = M

module Trivial: S with module Monad = Monadic.Identity

