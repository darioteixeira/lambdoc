(********************************************************************************)
(*	Idiosyncrasies.mli
	Copyright (c) 2009-2013 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type action_t = [ `Accept | `Deny ]

type 'a classifier_t = [ `Any | `Only of 'a | `Member of 'a list | `Not of 'a classifier_t ]

type feature_ruleset_t = (Feature.t classifier_t * action_t) list

type classname_ruleset_t = ((Feature.t classifier_t * Classname.t classifier_t) * action_t) list

type t =
	{
	feature_ruleset: feature_ruleset_t;
	feature_default: action_t;
	classname_ruleset: classname_ruleset_t;
	classname_default: action_t;
	max_macro_depth: int option;
	max_inline_depth: int option;
	max_block_depth: int option;
	}


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

val default: t

val make:
	?feature_ruleset:feature_ruleset_t ->
	?feature_default:action_t ->
	?classname_ruleset:classname_ruleset_t ->
	?classname_default:action_t ->
	?max_macro_depth:int option ->
	?max_inline_depth:int option ->
	?max_block_depth:int option ->
	unit ->
	t

