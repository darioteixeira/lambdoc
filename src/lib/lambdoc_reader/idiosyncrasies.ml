(********************************************************************************)
(*	Idiosyncrasies.ml
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core


(********************************************************************************)
(**	{1 Inner modules}							*)
(********************************************************************************)

module Feature_map = Map.Make (struct type t = Features.feature_t let compare = Pervasives.compare end)


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type t = bool Feature_map.t


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

let make ~accepted ~denied ~default =
	let internal_set = (Features.internal_features :> Features.feature_t list) in
	let public_set = (Features.public_features :> Features.feature_t list) in
	let base_map =
		let adder m x = Feature_map.add x true m in
		List.fold_left adder Feature_map.empty internal_set in
	let default_bool = (default = `Accept) in
	let is_accepted feature =
		if List.mem feature (denied :> Features.feature_t list)
		then
			false
		else
			if List.mem feature (accepted :> Features.feature_t list)
			then
				true
			else
				if List.mem feature public_set
				then default_bool
				else false in
	let make_feature map feature =
		Feature_map.add feature (is_accepted feature) map in
	List.fold_left make_feature base_map public_set


let check = Feature_map.find

