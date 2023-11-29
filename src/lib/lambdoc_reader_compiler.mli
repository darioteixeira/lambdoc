(** Compilation of a document AST.  These functions convert
    a document AST into a proper, final, ambivalent document.
*)

module Ast = Lambdoc_reader_ast
module Extension = Lambdoc_reader_extension

open Lambdoc_document
open Invalid


(********************************************************************************)
(** {1 Public functions and values}                                             *)
(********************************************************************************)

module Make: functor (Ext: Extension.S) ->
sig
    (** This will be raised if one of the command extensions misbehaves.
    *)
    exception Internal_extension_error of Ast.command

    (** Contextualize and (optionally) sort the errors by line number.
    *)
    val contextualize_errors:
        sort:bool ->
        string ->
        Error.localized list ->
        Error.contextualized list

    (** Compile a document AST into a manuscript.
    *)
    val compile:
        ?postprocessor:Error.localized list Ext.Foldmapper.t ->
        extcomms:Ext.extcomm list ->
        expand_entities:bool ->
        idiosyncrasies:Idiosyncrasies.t ->
        source:string ->
        Ast.t ->
        Ambivalent.t Ext.IO.t
end

