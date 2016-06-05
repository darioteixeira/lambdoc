(********************************************************************************)
(*  Lambdoc_reader_extension.mli
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Reader extension.
*)

module Ast = Lambdoc_reader_ast

open Lambdoc_prelude
open Lambdoc_core
open Basic


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type syntax =
    | Syn_empty                         (* No main parameters. Eg: Linebreak, Appendix *)
    | Syn_seq                           (* Parameter is an inline sequence. Eg: Bold, Paragraph *)
    | Syn_lit                           (* Parameter is a multiline sequence of raw text. Eg: Verbatim *)
    | Syn_frag                          (* Parameter is a fragment (list of blocks). Eg: Quote *)
    | Syn_raw of string                 (* Parameter is single sequence of raw text. Eg: Macroarg *)
    | Syn_raw_raw of string * string    (* Parameters are two sequences of raw text. Eg: Glyph, Picture *)
    | Syn_raw_seq of string             (* Parameters are a sequence of raw text followed by an inline sequence. Eg: Mref *)
    | Syn_raw_seqopt of string          (* Parameters are a sequence of raw text optionally followed by an inline sequence. Eg: Link *)


type extdef = ident * syntax


(********************************************************************************)
(** {1 Public signatures}                                                       *)
(********************************************************************************)

(** The signature of a reader extension.
*)
module type S =
sig
    module IO: Monad.S
    module Foldmapper: Foldmap.S with module IO = IO

    type ('a, 'b) result = [ `Okay of 'a | `Error of 'b ]
    type 'a function_result = ('a, Error.localized list) result
    type inline_function_result = (Ast.seq * Ast.frag) function_result IO.t
    type block_function_result = (Ast.frag * Ast.frag) function_result IO.t

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

module Make: functor (M: Monad.S) -> S with module IO = M

module Trivial: S with module IO = Monad.Identity

