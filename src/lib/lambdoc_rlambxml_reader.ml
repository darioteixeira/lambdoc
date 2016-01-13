(********************************************************************************)
(*  Lambdoc_rlambxml_reader.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license xmlt.
*)
(********************************************************************************)

module Make = Lambdoc_reader.Maker.Make (Lambdoc_rlambxml_readable)

module Trivial = Make (Lambdoc_reader.Extension.Trivial)

