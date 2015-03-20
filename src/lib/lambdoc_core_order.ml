(********************************************************************************)
(*	Lambdoc_core_order.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type ordinal_t = int

type hierarchical_t = int list

type 'a auto_given_t = [ `Auto_given of 'a ]
type 'a user_given_t = [ `User_given of 'a ]
type none_given_t = [ `None_given ]
type ('a, 'b) t = 'b constraint 'b = [< 'a auto_given_t | 'a user_given_t | none_given_t ]

