(********************************************************************************)
(*	Lambdoc_read_markdown.mli
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

include Lambdoc_reader.Reader.PARTIAL

module Simple: module type of Make (Lambdoc_reader.Extension.Unitary)

