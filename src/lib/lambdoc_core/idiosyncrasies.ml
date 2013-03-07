(********************************************************************************)
(*	Idiosyncrasies.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
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
	}


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

let make ?(feature_ruleset = []) ?(feature_default = `Accept) ?(classname_ruleset = []) ?(classname_default = `Accept) () =
	{feature_ruleset; feature_default; classname_ruleset; classname_default}

let default = make ()

