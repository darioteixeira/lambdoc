open Lambdoc_writer

module Make (Html: Html_sigs.NoWrap) =
struct
    module Writable = Lambdoc_whtml_writable.Make (Html)

    include Lambdoc_writer_maker.Make (Writable)
end

