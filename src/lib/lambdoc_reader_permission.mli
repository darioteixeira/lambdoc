(** Document permissions.
*)

module Ast = Lambdoc_reader_ast

open Lambdoc_document
open Invalid


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

val check_parameters:
    ?maybe_minipaged:bool option ->
    ?maybe_wrapped:bool option ->
    Ast.command ->
    Feature.t ->
    Error.msg list

val check_feature:
    Feature.t ->
    Idiosyncrasies.t ->
    bool

val check_classname:
    Feature.t ->
    Valid.classname ->
    Idiosyncrasies.t ->
    bool

