(********************************************************************************)
(*	Idiosyncrasies.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Basic

module List = BatList


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
(**	{1 Public functions and values}						*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Constructors}							*)
(********************************************************************************)

let make
	?(feature_ruleset = [])
	?(feature_default = `Accept)
	?(classname_ruleset = [])
	?(classname_default = `Accept)
	?(max_macro_depth = None)
	?(max_inline_depth = None)
	?(max_block_depth = None) () =
	{feature_ruleset; feature_default; classname_ruleset; classname_default; max_macro_depth; max_inline_depth; max_block_depth}


(********************************************************************************)
(**	{2 Built-in idiosyncrasies}						*)
(********************************************************************************)

let unrestricted =
	make ()

let restricted =
	make ~classname_default:`Deny ~max_macro_depth:(Some 1) ~max_inline_depth:(Some 1) ~max_block_depth:(Some 1) ()

let default =
	let classname_ruleset =
		[
		((`Only `Feature_paragraph, `Member ["initial"; "indent"; "noindent"]), `Accept);
		((`Only `Feature_itemize, `Member ["disc"; "circle"; "square"; "none"]), `Accept);
		((`Only `Feature_enumerate, `Member ["decimal"; "lower-roman"; "upper-roman"; "lower-alpha"; "upper-alpha"; "none"]), `Accept);
		((`Only `Feature_source, `Member ["plain"; "boxed"; "zebra"; "console"]), `Accept);
		((`Only `Feature_verbatim, `Member (List.init 10 string_of_int |> List.map (fun x -> "mult" ^ x))), `Accept);
		((`Only `Feature_picture, `Only "frame"), `Accept);
		((`Member [`Feature_verbatim; `Feature_picture; `Feature_pullquote; `Feature_custom; `Feature_equation; `Feature_printout; `Feature_table; `Feature_figure], `Member ["center"; "right"; "left"]), `Accept);
		] in
	make ~classname_ruleset ~classname_default:`Deny ~max_macro_depth:(Some 2) ~max_inline_depth:(Some 3) ~max_block_depth:(Some 3) ()

