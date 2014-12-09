(********************************************************************************)
(*	Extension.mli
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Reader extension.
*)

open Lambdoc_core
open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type inline_syntax_t =
	| Inlsyn_empty		(* No main parameters. Eg: Linebreak *)
	| Inlsyn_seq		(* Parameter is an inline sequence. Eg: Bold *)
	| Inlsyn_raw		(* Parameter is single sequence of raw text. Eg: Macroarg *)
	| Inlsyn_raw_raw	(* Parameters are two sequences of raw text. Eg: Glyph *)
	| Inlsyn_raw_seq	(* Parameters are a sequence of raw text followed by an inline sequence. Eg: Mref *)
	| Inlsyn_raw_seqopt	(* Parameters are a sequence of raw text followed optionally by an inline sequence. Eg: Link *)

type block_syntax_t =
	| Blksyn_empty		(* No main parameters. Eg: Appendix *)
	| Blksyn_seq		(* Parameter is an inline sequence. Eg: Paragraph *)
	| Blksyn_raw		(* Parameter is single sequence of raw text. *)
	| Blksyn_lit		(* Parameter is a multiline sequence of raw text. *)
	| Blksyn_frag		(* Parameter is a fragment (list of blocks). Eg: Quote *)
	| Blksyn_raw_raw	(* Parameters are two sequences of raw text. Eg: Picture *)

type inline_extdef_t = Ident.t * inline_syntax_t

type block_extdef_t = Ident.t * block_syntax_t


(********************************************************************************)
(**	{1 Public signatures}							*)
(********************************************************************************)

(**	The signature of a reader extension.
*)
module type S =
sig
	module Monad: Monadic.S

	type 'a result_t = [ `Okay of 'a | `Error of Error.error_msg_t list ]

	type link_reader_t = Href.t -> string result_t option Monad.t

	type image_reader_t = Href.t -> string result_t option Monad.t

	type inline_function_result_t = Ast.seq_t result_t Monad.t

	type block_function_result_t = Ast.frag_t result_t Monad.t

	type inline_function_t =
		| Inlfun_empty of (Ast.command_t -> inline_function_result_t)
		| Inlfun_seq of (Ast.command_t -> Ast.seq_t -> inline_function_result_t)
		| Inlfun_raw of (Ast.command_t -> string -> inline_function_result_t)
		| Inlfun_raw_raw of (Ast.command_t -> string -> string -> inline_function_result_t)
		| Inlfun_raw_seq of (Ast.command_t -> string -> Ast.seq_t -> inline_function_result_t)
		| Inlfun_raw_seqopt of (Ast.command_t -> string -> Ast.seq_t option -> inline_function_result_t)

	type block_function_t =
		| Blkfun_empty of (Ast.command_t -> block_function_result_t)
		| Blkfun_seq of (Ast.command_t -> Ast.seq_t -> block_function_result_t)
		| Blkfun_raw of (Ast.command_t -> string -> block_function_result_t)
		| Blkfun_lit of (Ast.command_t -> string -> block_function_result_t)
		| Blkfun_frag of (Ast.command_t -> Ast.frag_t -> block_function_result_t)
		| Blkfun_raw_raw of (Ast.command_t -> string -> string -> block_function_result_t)

	type inline_extcomm_t =
		{
		inltag: Ident.t;
		inlfun: inline_function_t;
		}

	type block_extcomm_t =
		{
		blktag: Ident.t;
		blkfun: block_function_t;
		blkcat: Blkcat.t list;
		}
end


(********************************************************************************)
(**	{1 Public modules}							*)
(********************************************************************************)

module Make: functor (M: Monadic.S) -> S with module Monad = M

module Trivial: S with module Monad = Monadic.Identity

