(********************************************************************************)
(*	Lambdoc_whtml5_writer.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_writer

module Make
	(Ext: Extension.S)
	(Html5: Html5_sigs.T with type 'a Xml.wrap = 'a and type 'a wrap = 'a and type 'a list_wrap = 'a list) =
struct
	module Writable = Lambdoc_whtml5_writable.Make (Html5)

	include Lambdoc_writer_maker.Make (Writable) (Ext)
end

module Make_trivial (Html5: Html5_sigs.T with type 'a Xml.wrap = 'a and type 'a wrap = 'a and type 'a list_wrap = 'a list) =
	Make (Extension.Trivial) (Html5)

