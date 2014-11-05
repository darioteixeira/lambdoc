(********************************************************************************)
(*	Extcomm.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Sexplib.Std
open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type syninl_t =
	[ `Syninl_simseq
	| `Syninl_simraw
	| `Syninl_simrawseq
	| `Syninl_simrawseqopt
	]

type synblksim_t = 
	[ `Synblk_simseq
	| `Synblk_simraw
	]

type synblkenv_t =
	[ `Synblk_envraw
	| `Synblk_envseqraw
	| `Synblk_envrawraw
	| `Synblk_envseqoptraw
	| `Synblk_envrawoptraw
	]

type synblk_t = [ synblksim_t | synblkenv_t ]

type extinl_t =
	| Extinl_simseq of Inline.seq_t
	| Extinl_simraw of string
	| Extinl_simrawseq of string * Inline.seq_t
	| Extinl_simrawseqopt of string * Inline.seq_t option
	with sexp

type extblk_t =
	| Extblk_simseq of Inline.seq_t
	| Extblk_simraw of string
	| Extblk_envraw of string
	| Extblk_envseqraw of Inline.seq_t * string
	| Extblk_envrawraw of string * string
	| Extblk_envseqoptraw of Inline.seq_t option * string
	| Extblk_envrawoptraw of string option * string
	with sexp

type extinldefs_t = (Ident.t * (syninl_t * bool)) list

type extblkdefs_t = (Ident.t * (synblk_t * Block.category_t)) list

