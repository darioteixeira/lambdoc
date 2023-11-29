type options = unit

module Make = Lambdoc_reader.Maker.Make (Lambdoc_rlambxml_readable)

module Trivial = Make (Lambdoc_reader.Extension.Trivial)

