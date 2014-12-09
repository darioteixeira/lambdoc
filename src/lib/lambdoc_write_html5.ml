(********************************************************************************)
(*	Lambdoc_write_html5.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

module Make
	(Ext: Lambdoc_writer.Extension.S)
	(Html5: Html5_sigs.T with type 'a Xml.wrap = 'a and type 'a wrap = 'a and type 'a list_wrap = 'a list) =
struct
	module Writable = Lambdoc_write_html5_impl.Main.Make (Html5)

	include Lambdoc_writer.Writer.Make (Writable) (Ext)
end

module Make_trivial (Html5: Html5_sigs.T with type 'a Xml.wrap = 'a and type 'a wrap = 'a and type 'a list_wrap = 'a list) =
	Make (Lambdoc_writer.Extension.Trivial) (Html5)

