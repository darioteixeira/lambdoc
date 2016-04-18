(********************************************************************************)
(*  Lambdoc_whtml_writer.ml
    Copyright (c) 2009-2016 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the GNU GPL version 2.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Lambdoc_writer

module Make (Html: Html_sigs.NoWrap) =
struct
    module Writable = Lambdoc_whtml_writable.Make (Html)

    include Lambdoc_writer_maker.Make (Writable)
end

