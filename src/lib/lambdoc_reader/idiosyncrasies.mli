(********************************************************************************)
(*	Idiosyncrasies.mli
	Copyright (c) 2009-2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_core
open Basic


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type t


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

val make:
	feature_ruleset:Features.feature_ruleset_t ->
	feature_default:Features.action_t ->
	classname_ruleset:Features.classname_ruleset_t ->
	classname_default:Features.action_t ->
	t

val check_feature: Features.feature_t -> t -> bool
val check_classname: Features.feature_t -> Classname.t -> t -> bool

