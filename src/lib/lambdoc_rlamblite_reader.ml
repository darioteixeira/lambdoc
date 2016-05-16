(********************************************************************************)
(*  Lambdoc_rlamblite_reader.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

type options = [ `Lambwiki | `Markdown ]

module Make = Lambdoc_reader.Maker.Make (Lambdoc_rlamblite_readable)

module Trivial = Make (Lambdoc_reader.Extension.Trivial)

