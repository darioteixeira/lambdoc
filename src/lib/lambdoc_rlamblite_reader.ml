type options = [ `Lambwiki | `Markdown ]

module Make = Lambdoc_reader.Maker.Make (Lambdoc_rlamblite_readable)

module Trivial = Make (Lambdoc_reader.Extension.Trivial)

