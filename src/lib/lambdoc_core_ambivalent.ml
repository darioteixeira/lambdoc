(********************************************************************************)
(*	Lambdoc_core_ambivalent.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type t =
	| Valid of Valid.t
	| Invalid of Invalid.t


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Constructors}							*)
(********************************************************************************)

let make_valid ~content ~bibs ~notes ~toc ~labels ~customs ~links ~images =
	Valid (Valid.make ~content ~bibs ~notes ~toc ~labels ~customs ~links ~images)

let make_invalid errors =
	Invalid (Invalid.make errors)

