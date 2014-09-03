(********************************************************************************)
(*	Lambdoc_read_lambxml.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license xmlt.
*)
(********************************************************************************)

include (Lambdoc_read_lambxml_impl.Main: Lambdoc_reader.Reader.PARTIAL)

module Simple = Lambdoc_read_lambxml_impl.Main.Make (Lambdoc_reader.Extension.Unit)

