(********************************************************************************)
(*	Idiosyncrasies.ml
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

module Feature_map =
	Map.Make (struct type t = Features.feature_t let compare = Pervasives.compare end)


type t = bool Feature_map.t


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

let make_idiosyncrasies feature_set accept_list deny_list default =
	let base_map =
		let features = (Features.available_internal_features :> Features.feature_t list) in
		let adder m x = Feature_map.add x true m
		in List.fold_left adder Feature_map.empty features in
	let default_bool =
		(default = `Accept) in
	let is_accepted feature =
		if List.mem feature deny_list
		then false
		else	 if List.mem feature accept_list
			then true
			else	if List.mem feature feature_set
				then default_bool
				else false in
	let make_feature map feature =
		Feature_map.add feature (is_accepted feature) map
	in List.fold_left make_feature base_map (Features.available_manuscript_features :> Features.feature_t list)


let make_composition_idiosyncrasies ?(accept_list = []) ?(deny_list = []) ?(default = `Accept) () =
	let composition_features = (Features.available_composition_features :> Features.feature_t list)
	and accept_list = (accept_list :> Features.feature_t list)
	and deny_list = (deny_list :> Features.feature_t list)
	in make_idiosyncrasies composition_features accept_list deny_list default


let make_manuscript_idiosyncrasies ?(accept_list = []) ?(deny_list = []) ?(default = `Accept) () =
	let manuscript_features = (Features.available_manuscript_features :> Features.feature_t list)
	and accept_list = (accept_list :> Features.feature_t list)
	and deny_list = (deny_list :> Features.feature_t list)
	in make_idiosyncrasies manuscript_features accept_list deny_list default


let check_feature feature map =
	Feature_map.find feature map

