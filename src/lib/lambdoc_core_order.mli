(********************************************************************************)
(*	Lambdoc_core_order.mli
	Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Definitions pertaining to document ordering.
*)


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type ordinal_t = int with sexp

type hierarchical_t = int list with sexp


(**	A block's ordering can be assigned by any of three sources: [`Auto_given] means that
	the ordering should be automatically given by the system; [`User_given] means that the
	ordering is manually given by the user; finally, when the block should not have any
	ordering at all, [`None_given] is used.  Note that different classes of blocks allow
	a different subset of these ordering variants.  Moreover, only the first variant must
	be parametrised over the actual ordering scheme used (as it makes no sense to talk of
	an ordering scheme when [`None_given] is used, for example).
*)

type 'a auto_given_t = [ `Auto_given of 'a ] with sexp
type 'a user_given_t = [ `User_given of 'a ] with sexp
type none_given_t = [ `None_given ] with sexp
type ('a, 'b) t = 'b constraint 'b = [< 'a auto_given_t | 'a user_given_t | none_given_t ] with sexp

