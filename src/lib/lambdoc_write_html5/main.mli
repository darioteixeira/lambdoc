(********************************************************************************)
(*	Main.mli
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Eliom_content
open Lambdoc_writer

include Writer.S with type t = [ `Div ] Html5.F.elt

