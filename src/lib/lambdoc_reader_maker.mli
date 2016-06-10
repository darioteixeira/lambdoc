(********************************************************************************)
(*  Lambdoc_reader_maker.mli
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

(** Document reader.
*)

module Ast = Lambdoc_reader_ast
module Extension = Lambdoc_reader_extension

open Lambdoc_document
open Invalid


(********************************************************************************)
(** {1 Public signatures}                                                       *)
(********************************************************************************)

(** The signature that all wannabe document readers must implement.
*)
module type READABLE =
sig
    type options

    val ast_from_string:
        ?options:options ->
        linenum_offset:int ->
        inline_extdefs:Extension.extdef list ->
        block_extdefs:Extension.extdef list ->
        string ->
        [ `Okay of Ast.t | `Error of Error.reading list ]
end


(** The signature of a document reader.
*)
module type READER =
sig
    module Ext: Extension.S

    type options

    val ambivalent_from_string:
        ?options:options ->
        ?postprocessor:Error.localized list Ext.Foldmapper.t ->
        ?extcomms:Ext.extcomm list ->
        ?linenum_offset:int ->
        ?verify_utf8:bool ->
        ?expand_entities:bool ->
        ?idiosyncrasies:Idiosyncrasies.t ->
        string ->
        Ambivalent.t Ext.IO.t
end


(** The complete signature for a reader with ancillary modules.
*)
module type FULL =
sig
    type options

    module Make: functor (Ext: Extension.S) -> READER with module Ext = Ext and type options = options

    module Trivial: READER with module Ext = Extension.Trivial and type options = options
end


(********************************************************************************)
(** {1 Public modules and functors}                                             *)
(********************************************************************************)

module Make:
    functor (Readable: READABLE) ->
    functor (Ext: Extension.S) -> READER with
    module Ext = Ext and
    type options = Readable.options

