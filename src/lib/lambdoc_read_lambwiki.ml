(********************************************************************************)
(*	Lambdoc_read_lambwiki.ml
	Copyright (c) 2009-2014 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

module Make = Lambdoc_read_lambwiki_impl.Main.Make

module Trivial = Make (Lambdoc_reader.Extension.Trivial)

