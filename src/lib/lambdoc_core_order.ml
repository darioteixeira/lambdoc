(********************************************************************************)
(*	Lambdoc_core_order.ml
	Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Sexplib.Std


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type ordinal_t = int with sexp

type hierarchical_t = int list with sexp

type 'a auto_given_t = [ `Auto_given of 'a ] with sexp
type 'a user_given_t = [ `User_given of 'a ] with sexp
type none_given_t = [ `None_given ] with sexp
type ('a, 'b) t = 'b constraint 'b = [< 'a auto_given_t | 'a user_given_t | none_given_t ] with sexp

