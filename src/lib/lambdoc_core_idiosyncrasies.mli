(********************************************************************************)
(*  Lambdoc_core_idiosyncrasies.mli
    Copyright (c) 2009-2015 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Definition of document idiosyncrasies.  These represent additional
    restrictions over the set of allowed document elements and classnames.
*)

module Basic = Lambdoc_core_basic
module Feature = Lambdoc_core_feature

open Basic


(********************************************************************************)
(** {1 Type definitions}                                                        *)
(********************************************************************************)

type action = [ `Accept | `Deny ] [@@deriving sexp]

type 'a classifier = [ `Any | `Only of 'a | `Member of 'a list | `Not of 'a classifier ] [@@deriving sexp]

type feature_ruleset = (Feature.t classifier * action) list [@@deriving sexp]

type classname_ruleset = ((Feature.t classifier * classname classifier) * action) list [@@deriving sexp]

type t =
    {
    feature_ruleset: feature_ruleset;
    feature_default: action;
    classname_ruleset: classname_ruleset;
    classname_default: action;
    max_macro_depth: int option;
    max_inline_depth: int option;
    max_block_depth: int option;
    } [@@deriving sexp]


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

(********************************************************************************)
(** {2 Constructors}                                                            *)
(********************************************************************************)

val make:
    ?feature_ruleset:feature_ruleset ->
    ?feature_default:action ->
    ?classname_ruleset:classname_ruleset ->
    ?classname_default:action ->
    ?max_macro_depth:int option ->
    ?max_inline_depth:int option ->
    ?max_block_depth:int option ->
    unit ->
    t


(********************************************************************************)
(** {2 Built-in idiosyncrasies}                                                 *)
(********************************************************************************)

val unrestricted: t (** Maximally unrestrictive: all features and classnames allowed, and there are no depth limits *)
val restricted: t   (** Maximally restrictive: no features and no classnames allowed, and maximum depth of 1 *)
val default: t      (** Unrestrictive on features, but only allows the classnames shipped with default CSS and has reasonable depth limits *)

