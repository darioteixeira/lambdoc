module Ast = Lambdoc_reader_ast

open Lambdoc_prelude
open Lambdoc_document
open Valid
open Invalid


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

module Make (M: Monad.S): S with module IO = M =
struct
    module IO = M
    module Foldmapper = Foldmap.Make (M)

    type ('a, 'b) result = [ `Okay of 'a | `Error of 'b ]
    type reader_result = (string, Error.msg list) result
    type 'a function_result = ('a, Error.localized list) result
    type link_reader = href -> reader_result option IO.t
    type image_reader = href -> reader_result option IO.t
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


module Trivial = Make (Monad.Identity)

