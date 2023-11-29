(** Main interface to the Lambtex reader.
*)

type options = unit

module Make = Lambdoc_reader.Maker.Make (Lambdoc_rlambtex_readable)

module Trivial = Make (Lambdoc_reader.Extension.Trivial)

