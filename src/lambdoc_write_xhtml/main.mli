(********************************************************************************)
(*	Main.mli
	Copyright (c) 2009 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_writer

include Writer.S with type t = [ `Div ] XHTML.M.elt
