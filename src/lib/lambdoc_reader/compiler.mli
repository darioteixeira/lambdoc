(********************************************************************************)
(*	Compiler.mli
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Compilation of a document AST.  These functions convert
	a document AST into a proper, final, ambivalent document.
*)

open Lambdoc_core


(********************************************************************************)
(**	{1 Public functions and values}						*)
(********************************************************************************)

module Make: functor (Ext: Extension.S) ->
sig
	(**	Process and (optionally) sort the errors by line number.
	*)
	val process_errors:
		sort:bool ->
		string ->
		(int option * Error.error_msg_t) list ->
		Error.t list

	(**	Compile a document AST into a manuscript.
	*)
	val compile:
		?rconfig:Ext.rconfig_t ->
		expand_entities:bool ->
		idiosyncrasies:Idiosyncrasies.t ->
		source:string ->
		Ast.t ->
		(Ext.linkdata_t, Ext.imagedata_t, Ext.extinldata_t, Ext.extblkdata_t) Ambivalent.t Ext.Monad.t
end

