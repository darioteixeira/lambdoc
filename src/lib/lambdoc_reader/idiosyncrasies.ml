(********************************************************************************)
(*	Idiosyncrasies.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type t =
	{
	feature_ruleset: Features.feature_ruleset_t;
	feature_default: Features.action_t;
	classname_ruleset: Features.classname_ruleset_t;
	classname_default: Features.action_t;
	}


(********************************************************************************)
(**	{1 Private functions and values}					*)
(********************************************************************************)

let classify = function
	| (_, `Any)		-> true
	| (x, `Only target)	-> (x = target)
	| (x, `Member targets)	-> List.mem x targets


let check verify ruleset default =
	let rec iterate = function
		| []	   -> default
		| (rule, action) :: tl -> if verify rule then action else iterate tl
	in match iterate ruleset with
		| `Accept -> true
		| `Deny	  -> false


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

let make ~feature_ruleset ~feature_default ~classname_ruleset ~classname_default =
	{feature_ruleset; feature_default; classname_ruleset; classname_default}


let check_feature feature {feature_ruleset; feature_default; _} =
	let verify rule =
		classify (feature, rule) in
	check verify feature_ruleset feature_default


let check_classname feature classname {classname_ruleset; classname_default; _} =
	let verify (rule1, rule2) =
		classify (feature, rule1) && classify (classname, rule2) in
	check verify classname_ruleset classname_default

